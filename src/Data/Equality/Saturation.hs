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

import qualified Data.Map    as M
import qualified Data.IntMap as IM

import Data.Functor.Classes
import Data.Traversable
import Control.Monad
import Control.Monad.State

import Data.Fix

import Data.Equality.Graph
import Data.Equality.Matching
import Data.Equality.Extraction

data Rewrite lang = Pattern lang := Pattern lang
    deriving (Eq, Ord)
infix 3 :=

data Stat = Stat { bannedUntil :: Int
                 , timesBanned :: Int
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
        -- backoff scheduling
        equalitySaturation' :: Language l => Int -> M.Map (Rewrite l) Stat -> EGS l ()
        equalitySaturation' 30 _ = return () -- Stop after X iterations
        equalitySaturation' i stats = do

            EGraph { memo = beforeMemo, classes = beforeClasses } <- get

            -- Read-only phase, invariants are preserved
            -- With backoff scheduler
            (matches, newStats) <- mconcat <$> forM rewrites \(lhs := rhs) -> do 
                case M.lookup (lhs := rhs) stats of
                  -- If it's banned until some iteration, don't match this rule
                  -- against anything.
                  Just s | i < bannedUntil s -> return ([], stats)

                  -- Otherwise, match and update stats
                  x -> do

                      -- Match pattern
                      matches' <- ematchM lhs -- Add rewrite to the e-match substitutions

                      -- Backoff scheduler: update stats
                      let newStats = updateStats i (lhs:=rhs) x stats matches'

                      return (map (lhs := rhs,) matches', newStats)

            -- Write-only phase, temporarily break invariants
            forM_ matches \case
                (_ := VariablePattern v, (subst, eclass)) -> do
                    -- rhs is equal to right hand side, simply merge class where lhs
                    -- pattern was found (@eclass@) and the eclass the pattern variable
                    -- matched (@lookup v subst@)
                    case lookup v subst of
                      Nothing -> error "impossible: couldn't find v in subst"
                      Just n  -> merge n eclass
                (_ := NonVariablePattern rhs, (subst, eclass)) -> do
                    -- rhs is (at the top level) a non-variable pattern, so substitute
                    -- all pattern variables in the pattern and create a new e-node (and
                    -- e-class that represents it), then merge the e-class of the
                    -- substituted rhs with the class that matched the left hand side
                    eclass' <- reprPat subst rhs
                    merge eclass eclass'

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


        -- | Represent a pattern in the e-graph a pattern given substitions
        reprPat :: Language l
                => Subst -> l (Pattern l) -> EGS l ClassId
        reprPat subst = add . Node <=< traverse \case
            VariablePattern v ->
                case lookup v subst of
                    Nothing -> error "impossible: couldn't find v in subst?"
                    Just i  -> return i
            NonVariablePattern p -> reprPat subst p


-- | Backoff scheduler: update stats
updateStats :: Ord1 l
            => Int                    -- ^ Iteration we're in
            -> Rewrite l              -- ^ Rewrite rule we're updating
            -> Maybe Stat             -- ^ Current stat for this rewrite rule (we already got it so no point in doing a lookup again)
            -> M.Map (Rewrite l) Stat -- ^ The map to update
            -> [Match]                -- ^ The list of matches resulting from matching this rewrite rule
            -> M.Map (Rewrite l) Stat -- ^ The updated map
updateStats i rw currentStat stats matches =

    if total_len > threshold

      then
        M.alter updateBans rw stats

      else
        stats

    where

      total_len = sum (map length matches)

      defaultMatchLimit = 32
      defaultBanLength  = 5

      bannedN = case currentStat of
                  Nothing -> 0;
                  Just (timesBanned -> n) -> n

      threshold = defaultMatchLimit * 2^bannedN

      ban_length = defaultBanLength * 2^bannedN;

      updateBans = \case
        Nothing -> Just (Stat (i + ban_length) 1)
        Just (Stat _ n)  -> Just (Stat (i + ban_length) (n+1))

