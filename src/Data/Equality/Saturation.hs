{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-|
  Given an input program 𝑝, equality saturation constructs an e-graph 𝐸 that
  represents a large set of programs equivalent to 𝑝, and then extracts the
  “best” program from 𝐸.

  The e-graph is grown by repeatedly applying pattern-based rewrites.
  Critically, these rewrites only add information to the e-graph, eliminating
  the need for careful ordering.

  Upon reaching a fixed point (saturation), 𝐸 will represent all equivalent
  ways to express 𝑝 with respect to the given rewrites.

  After saturation (or timeout), a final extraction procedure analyzes 𝐸 and
  selects the optimal program according to a user-provided cost function.
 -}
module Data.Equality.Saturation
    (
      -- * Equality saturation
      equalitySaturation, equalitySaturation'

      -- * Re-exports for equality saturation

      -- ** Writing rewrite rules
    , Rewrite(..), RewriteCondition

      -- ** Writing cost functions
      --
      -- | 'CostFunction' re-exported from 'Data.Equality.Extraction' since they are required to do equality saturation
    , CostFunction --, Cost, depthCost

      -- ** Writing expressions
      -- 
      -- | Expressions must be written in their fixed-point form, since the
      -- 'Language' must be given in its base functor form
    , Fix(..), cata

    ) where

import qualified Data.IntMap.Strict as IM

import Data.Bifunctor
import Control.Monad

import Data.Proxy

import Data.Equality.Utils
import qualified Data.Equality.Graph as G
import Data.Equality.Graph.Monad
import Data.Equality.Language
import Data.Equality.Graph.Classes
import Data.Equality.Matching
import Data.Equality.Matching.Database
import Data.Equality.Extraction

import Data.Equality.Saturation.Rewrites
import Data.Equality.Saturation.Scheduler

-- | Equality saturation with defaults
equalitySaturation :: forall l. Language l
                   => Fix l             -- ^ Expression to run equality saturation on
                   -> [Rewrite l]       -- ^ List of rewrite rules
                   -> CostFunction l    -- ^ Cost function to extract the best equivalent representation
                   -> (Fix l, EGraph l) -- ^ Best equivalent expression and resulting e-graph
equalitySaturation = equalitySaturation' (Proxy @BackoffScheduler)


-- | Run equality saturation on an expression given a list of rewrites, and
-- extract the best equivalent expression according to the given cost function
--
-- This variant takes all arguments instead of using defaults
equalitySaturation' :: forall l schd
                    . (Language l, Scheduler schd)
                    => Proxy schd        -- ^ Proxy for the scheduler to use
                    -> Fix l             -- ^ Expression to run equality saturation on
                    -> [Rewrite l]       -- ^ List of rewrite rules
                    -> CostFunction l    -- ^ Cost function to extract the best equivalent representation
                    -> (Fix l, EGraph l) -- ^ Best equivalent expression and resulting e-graph
equalitySaturation' _ expr rewrites cost = egraph $ do

    -- Represent expression as an e-graph
    origClass <- represent expr

    -- Run equality saturation (by applying non-destructively all rewrites)
    equalitySaturation'' 0 mempty -- Start at iteration 0

    -- Extract best solution from the e-class of the original expression
    gets $ \g -> extractBest g cost origClass

      where

        -- Take map each rewrite rule to stats on its usage so we can do
        -- backoff scheduling. Each rewrite rule is assigned an integer
        -- (corresponding to its position in the list of rewrite rules)
        equalitySaturation'' :: Int -> IM.IntMap (Stat schd) -> EGraphM l ()
        equalitySaturation'' 30 _ = return () -- Stop after X iterations
        equalitySaturation'' i stats = do

            egr@G.EGraph{ G.memo = beforeMemo, G.classes = beforeClasses } <- get

            let db = eGraphToDatabase egr

            -- Read-only phase, invariants are preserved
            -- With backoff scheduler
            -- ROMES:TODO parMap with chunks
            let (!matches, newStats) = mconcat (fmap (matchWithScheduler db i stats) (zip [1..] rewrites))

            -- Write-only phase, temporarily break invariants
            forM_ matches applyMatchesRhs

            -- Restore the invariants once per iteration
            rebuild
            
            G.EGraph { G.memo = afterMemo, G.classes = afterClasses } <- get

            -- ROMES:TODO: Node limit...
            -- ROMES:TODO: Actual Timeout... not just iteration timeout
            -- ROMES:TODO Better saturation (see Runner)
            -- Apply rewrites until saturated or ROMES:TODO: timeout
            unless (G.sizeNM afterMemo == G.sizeNM beforeMemo
                      && IM.size afterClasses == IM.size beforeClasses)
                (equalitySaturation'' (i+1) newStats)

        matchWithScheduler :: Database l -> Int -> IM.IntMap (Stat schd) -> (Int, Rewrite l) -> ([(Rewrite l, Match)], IM.IntMap (Stat schd))
        matchWithScheduler db i stats = \case
            (rw_id, rw :| cnd) -> first (map (first (:| cnd))) $ matchWithScheduler db i stats (rw_id, rw)
            (rw_id, lhs := rhs) -> do
                case IM.lookup rw_id stats of
                  -- If it's banned until some iteration, don't match this rule
                  -- against anything.
                  Just s | isBanned @schd i s -> ([], stats)

                  -- Otherwise, match and update stats
                  x -> do

                      -- Match pattern
                      let matches' = ematch db lhs -- Add rewrite to the e-match substitutions

                      -- Backoff scheduler: update stats
                      let newStats = updateStats @schd i rw_id x stats matches'

                      (map (lhs := rhs,) matches', newStats)

        applyMatchesRhs :: (Rewrite l, Match) -> EGraphM l ()
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
                    case IM.lookup v subst of
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
        reprPat :: Subst -> l (Pattern l) -> EGraphM l ClassId
        reprPat subst = add . G.Node <=< traverse \case
            VariablePattern v ->
                case IM.lookup v subst of
                    Nothing -> error "impossible: couldn't find v in subst?"
                    Just i  -> return i
            NonVariablePattern p -> reprPat subst p
{-# SCC equalitySaturation' #-}

