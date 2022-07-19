{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module Data.Equality.Saturation
    ( module Data.Equality.Saturation
    , Fix(..), foldFix, unfoldFix
    , Cost
    ) where

import GHC.Conc

import qualified Data.Map    as M
import qualified Data.IntMap as IM

import Data.Bifunctor
import Control.Monad
import Control.Monad.State

import Data.Fix

import Data.Equality.Graph
import Data.Equality.Matching
import Database
import Data.Equality.Extraction

data Rewrite lang = Pattern lang := Pattern lang
                  | Rewrite lang :| RewriteCondition lang -- Conditional rewrites
infix 3 :=
infixl 2 :|

-- | A rewrite condition. With a substitution from bound variables to e-classes
-- and with the e-graph, return true when a condition is satisfied
type RewriteCondition lang = Subst -> EGraph lang -> Bool

data Stat = Stat { bannedUntil :: {-# UNPACK #-} !Int
                 , timesBanned :: {-# UNPACK #-} !Int
                 } deriving Show

equalitySaturation :: forall l. Language l
                   => Fix l -> [Rewrite l] -> (l Cost -> Cost) -> (Fix l, EGraph l)
equalitySaturation expr rewrites cost = runEGS emptyEGraph $ do

    -- Represent expression as an e-graph
    origClass <- represent expr

    -- Run equality saturation
    equalitySaturation' 0 mempty -- Start at iteration 0

    -- Extract best solution from the e-class of the original expression
    g <- get
    return $ extractBest g cost origClass

      where

        -- Take map each rewrite rule to stats on its usage so we can do
        -- backoff scheduling. Each rewrite rule is assigned an integer
        -- (corresponding to its position in the list of rewrite rules)
        equalitySaturation' :: Int -> IM.IntMap Stat -> EGS l ()
        equalitySaturation' 30 _ = return () -- Stop after X iterations
        equalitySaturation' i stats = do

            egr@EGraph{ memo = beforeMemo, classes = beforeClasses } <- get

            let db = eGraphToDatabase egr

            -- Read-only phase, invariants are preserved
            -- With backoff scheduler
            let (matches, newStats) = mconcat (parMap (matchWithBackoffScheduler db i stats) (zip [1..] rewrites))

            -- Write-only phase, temporarily break invariants
            forM_ matches applyMatchesRhs

            -- Restore the invariants once per iteration
            rebuild
            
            EGraph { memo = afterMemo, classes = afterClasses } <- get

            -- ROMES:TODO: Node limit...
            -- ROMES:TODO: Actual Timeout... not just iteration timeout
            -- ROMES:TODO Better saturation (see Runner)
            -- Apply rewrites until saturated or ROMES:TODO: timeout
            unless (M.size afterMemo == M.size beforeMemo
                      && IM.size afterClasses == IM.size beforeClasses)
                (equalitySaturation' (i+1) newStats)

        matchWithBackoffScheduler :: Database l -> Int -> IM.IntMap Stat -> (Int, Rewrite l) -> ([(Rewrite l, Match)], IM.IntMap Stat)
        matchWithBackoffScheduler db i stats = \case
            (rw_id, rw :| cnd) -> first (map (first (:| cnd))) $ matchWithBackoffScheduler db i stats (rw_id, rw)
            (rw_id, lhs := rhs) -> do 
                case IM.lookup rw_id stats of
                  -- If it's banned until some iteration, don't match this rule
                  -- against anything.
                  Just s | i < bannedUntil s -> ([], stats)

                  -- Otherwise, match and update stats
                  x -> do

                      -- Match pattern
                      let matches' = ematch db lhs -- Add rewrite to the e-match substitutions

                      -- Backoff scheduler: update stats
                      let newStats = updateStats i rw_id x stats matches'

                      (map (lhs := rhs,) matches', newStats)

        applyMatchesRhs :: (Rewrite l, Match) -> EGS l ()
        applyMatchesRhs =
            \case
                (rw :| cond, m@(Match subst _)) -> do
                    -- If the rewrite condition is satisfied, applyMatchesRhs on the rewrite rule.
                    egr <- get
                    when (cond subst egr) $
                       applyMatchesRhs (rw, m)

                (_ := VariablePattern v, Match subst eclass) -> do
                    -- rhs is equal to a variable, simply merge class where lhs
                    -- pattern was found (@eclass@) and the eclass the pattern
                    -- variable matched (@lookup v subst@)
                    case lookup v subst of
                      Nothing -> error "impossible: couldn't find v in subst"
                      Just n  -> do
                          _ <- merge n eclass
                          return ()
                (_ := NonVariablePattern rhs, Match subst eclass) -> do
                    -- rhs is (at the top level) a non-variable pattern, so substitute
                    -- all pattern variables in the pattern and create a new e-node (and
                    -- e-class that represents it), then merge the e-class of the
                    -- substituted rhs with the class that matched the left hand side
                    eclass' <- reprPat subst rhs
                    _ <- merge eclass eclass'
                    return ()

        -- | Represent a pattern in the e-graph a pattern given substitions
        reprPat :: Subst -> l (Pattern l) -> EGS l ClassId
        reprPat subst = add . Node <=< traverse \case
            VariablePattern v ->
                case lookup v subst of
                    Nothing -> error "impossible: couldn't find v in subst?"
                    Just i  -> return i
            NonVariablePattern p -> reprPat subst p


-- | Backoff scheduler: update stats
updateStats :: Int            -- ^ Iteration we're in
            -> Int            -- ^ Index of rewrite rule we're updating
            -> Maybe Stat     -- ^ Current stat for this rewrite rule (we already got it so no point in doing a lookup again)
            -> IM.IntMap Stat -- ^ The map to update
            -> [Match]        -- ^ The list of matches resulting from matching this rewrite rule
            -> IM.IntMap Stat -- ^ The updated map
updateStats i rw currentStat stats matches =

    if total_len > threshold

      then
        IM.alter updateBans rw stats

      else
        stats

    where

      -- TODO: Overall difficult, and buggy at the moment.
      total_len = sum (map (length . matchSubst) matches)

      defaultMatchLimit = 140 -- they're using 1000...
      defaultBanLength  = 10

      bannedN = case currentStat of
                  Nothing -> 0;
                  Just (timesBanned -> n) -> n

      threshold = defaultMatchLimit * (2^bannedN)

      ban_length = defaultBanLength * (2^bannedN)

      updateBans = \case
        Nothing -> Just (Stat (i + ban_length) 1)
        Just (Stat _ n)  -> Just (Stat (i + ban_length) (n+1))


-- We don't have the parallel package, so roll our own simple parMap
parMap _ [] = []
parMap f (x:xs) = fx `par` (fxs `pseq` (fx : fxs))
    where fx = f x; fxs = parMap f xs
