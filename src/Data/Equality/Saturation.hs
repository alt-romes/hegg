{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
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
      equalitySaturation, equalitySaturation', runEqualitySaturation

      -- * Re-exports for equality saturation

      -- ** Writing rewrite rules
    , Rewrite(..), RewriteCondition

      -- ** Writing cost functions
      --
      -- | 'CostFunction' re-exported from 'Data.Equality.Extraction' since they are required to do equality saturation
    , CostFunction --, depthCost

      -- ** Writing expressions
      -- 
      -- | Expressions must be written in their fixed-point form, since the
      -- 'Language' must be given in its base functor form
    , Fix(..), cata

    ) where

import qualified Data.IntMap.Strict as IM

import Control.Monad

import Data.Equality.Utils
import Data.Equality.Graph.Nodes
import Data.Equality.Graph.Lens
import Data.Equality.Graph.Internal (EGraph(classes))
import qualified Data.Equality.Graph as G
import Data.Equality.Graph.Monad
import Data.Equality.Language
import Data.Equality.Analysis
import Data.Equality.Graph.Classes
import Data.Equality.Matching
import Data.Equality.Matching.Database
import Data.Equality.Extraction

import Data.Equality.Saturation.Rewrites
import Data.Equality.Saturation.Scheduler

-- | Equality saturation with defaults
equalitySaturation :: forall a l cost
                    . (Analysis a l, Language l, Ord cost)
                   => Fix l               -- ^ Expression to run equality saturation on
                   -> [Rewrite a l]         -- ^ List of rewrite rules
                   -> CostFunction l cost -- ^ Cost function to extract the best equivalent representation
                   -> (Fix l, EGraph a l)   -- ^ Best equivalent expression and resulting e-graph
equalitySaturation = equalitySaturation' defaultBackoffScheduler


-- | Run equality saturation on an expression given a list of rewrites, and
-- extract the best equivalent expression according to the given cost function
--
-- This variant takes all arguments instead of using defaults
equalitySaturation' :: forall a l schd cost
                    . (Analysis a l, Language l, Scheduler l schd, Ord cost)
                    => schd                -- ^ Scheduler to use
                    -> Fix l               -- ^ Expression to run equality saturation on
                    -> [Rewrite a l]       -- ^ List of rewrite rules
                    -> CostFunction l cost -- ^ Cost function to extract the best equivalent representation
                    -> (Fix l, EGraph a l)   -- ^ Best equivalent expression and resulting e-graph
equalitySaturation' schd expr rewrites cost = egraph $ do

    -- Represent expression as an e-graph
    origClass <- represent expr

    -- Run equality saturation (by applying non-destructively all rewrites)
    runEqualitySaturation schd rewrites

    -- Extract best solution from the e-class of the original expression
    gets $ \g -> extractBest g cost origClass
{-# INLINABLE equalitySaturation' #-}


-- | Run equality saturation on an e-graph by non-destructively applying all
-- given rewrite rules until saturation (using the given 'Scheduler')
runEqualitySaturation :: forall a l schd
                       . (Analysis a l, Language l, Scheduler l schd)
                      => schd                -- ^ Scheduler to use
                      -> [Rewrite a l]       -- ^ List of rewrite rules
                      -> EGraphM a l ()
runEqualitySaturation schd rewrites = runEqualitySaturation' 0 mempty where -- Start at iteration 0

  -- Take map each rewrite rule to stats on its usage so we can do
  -- backoff scheduling. Each rewrite rule is assigned an integer
  -- (corresponding to its position in the list of rewrite rules)
  runEqualitySaturation' :: Int -> IM.IntMap (Stat l schd) -> EGraphM a l ()
  runEqualitySaturation' 30 _ = return () -- Stop after X iterations
  runEqualitySaturation' i stats = do

      egr <- get

      let (beforeMemo, beforeClasses) = (egr^._memo, classes egr)
          db = eGraphToDatabase egr

      -- Read-only phase, invariants are preserved
      -- With backoff scheduler
      -- ROMES:TODO parMap with chunks
      let (!matches, newStats) = mconcat (fmap (\(rw_id,rw) ->
            let (ms, ss, vss) = matchWithScheduler db i stats rw_id rw
             in (map (\m -> (rw,m,vss)) ms, ss)) (zip [1..] rewrites))

      -- Write-only phase, temporarily break invariants
      forM_ matches applyMatchesRhs

      -- Restore the invariants once per iteration
      rebuild
      
      (afterMemo, afterClasses) <- gets (\g -> (g^._memo, classes g))

      -- ROMES:TODO: Node limit...
      -- ROMES:TODO: Actual Timeout... not just iteration timeout
      -- ROMES:TODO Better saturation (see Runner)
      -- Apply rewrites until saturated or ROMES:TODO: timeout
      unless (G.sizeNM afterMemo == G.sizeNM beforeMemo
                && IM.size afterClasses == IM.size beforeClasses)
          (runEqualitySaturation' (i+1) newStats)

  matchWithScheduler :: Database l -> Int -> IM.IntMap (Stat l schd) -> Int -> Rewrite a l
                     -> ([Match], IM.IntMap (Stat l schd), VarsState {- the vars mapping resulting from compiling the query -})
  matchWithScheduler db i stats rw_id rw = case rw of
      rw' :| _ -> matchWithScheduler db i stats rw_id rw'
      lhs := _ -> do
          let (lhs_query, varsState) = compileToQuery lhs

          case IM.lookup rw_id stats of
            -- If it's banned until some iteration, don't match this rule
            -- against anything.
            Just s | isBanned @l @schd i s -> ([], stats, varsState)

            -- Otherwise, match and update stats
            x -> do

                -- Match pattern
                let matches' = ematch db lhs_query -- Add rewrite to the e-match substitutions

                -- Some scheduler: update stats
                let newStats = updateStats schd i rw_id rw x stats matches'

                (matches', newStats, varsState)

  applyMatchesRhs :: (Rewrite a l, Match, VarsState) -> EGraphM a l ()
  applyMatchesRhs =
      \case
          (rw :| cond, m@(Match subst _), vss) -> do
              -- If the rewrite condition is satisfied, applyMatchesRhs on the rewrite rule.
              egr <- get
              when (cond vss subst egr) $
                 applyMatchesRhs (rw, m, vss)

          (_ := VariablePattern v, Match subst eclass, vss) -> do
              -- rhs is equal to a variable, simply merge class where lhs
              -- pattern was found (@eclass@) and the eclass the pattern
              -- variable matched (@lookup v subst@)
              let n = findSubst (findVarName vss v) subst
              _ <- merge n eclass
              return ()

          (_ := NonVariablePattern rhs, Match subst eclass, vss) -> do
              -- rhs is (at the top level) a non-variable pattern, so substitute
              -- all pattern variables in the pattern and create a new e-node (and
              -- e-class that represents it), then merge the e-class of the
              -- substituted rhs with the class that matched the left hand side
              eclass' <- reprPat vss subst rhs
              _ <- merge eclass eclass'
              return ()

  -- | Represent a pattern in the e-graph a pattern given substitions
  reprPat :: VarsState -> Subst -> l (Pattern l) -> EGraphM a l ClassId
  reprPat vss subst = add . Node <=< traverse \case
      VariablePattern v -> pure $ findSubst (findVarName vss v) subst
      NonVariablePattern p -> reprPat vss subst p
{-# INLINEABLE runEqualitySaturation #-}

