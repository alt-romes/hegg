{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
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
    , Rewrite(..), RewriteCondition, PatternRewriteFun, rewriteLhs, rewriteRhs

      -- ** Writing cost functions
      --
      -- | 'CostFunction' re-exported from 'Data.Equality.Extraction' since they are required to do equality saturation
    , CostFunction, costOnly --, depthCost

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
                   => Fix l                 -- ^ Expression to run equality saturation on
                   -> [Rewrite a l]         -- ^ List of rewrite rules
                   -> CostFunction a l cost -- ^ Cost function to extract the best equivalent representation
                   -> (Fix l, EGraph a l)   -- ^ Best equivalent expression and resulting e-graph
equalitySaturation = equalitySaturation' defaultBackoffScheduler


-- | Run equality saturation on an expression given a list of rewrites, and
-- extract the best equivalent expression according to the given cost function
--
-- This variant takes all arguments instead of using defaults
equalitySaturation' :: forall a l schd cost
                    . (Analysis a l, Language l, Scheduler l schd, Ord cost)
                    => schd                  -- ^ Scheduler to use
                    -> Fix l                 -- ^ Expression to run equality saturation on
                    -> [Rewrite a l]         -- ^ List of rewrite rules
                    -> CostFunction a l cost -- ^ Cost function to extract the best equivalent representation
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
--
-- Rebuild after each round of applications.
-- A fixed point requires unchanged class nodes and analysis as well as stable
-- node/class counts. Execution is bounded to 30 rounds. A computed builder
-- returning 'Nothing' may be called again as rounds continue, subject to the
-- scheduler's bans and the iteration limit.
runEqualitySaturation :: forall a l schd
                       . (Analysis a l, Language l, Scheduler l schd)
                      => schd                -- ^ Scheduler to use
                      -> [Rewrite a l]       -- ^ List of rewrite rules
                      -> EGraphM a l ()
runEqualitySaturation schd rewrites = runEqualitySaturation' 0 mempty where

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
            let (ms, ss, vss) = matchWithScheduler egr db i stats rw_id rw
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
      let structChanged = G.sizeNM afterMemo /= G.sizeNM beforeMemo
                       || IM.size afterClasses /= IM.size beforeClasses
          saturated = not structChanged
                   -- Builders may become applicable after analysis changes
                   -- or pruning replaces nodes without changing either count.
                   && IM.isSubmapOfBy sameClass beforeClasses afterClasses
          -- Only consider retrying if no rules matched at all (likely because
          -- they're all banned). If rules matched but didn't add new structure,
          -- unbanning more rules probably won't help.
          noRulesMatched = null matches
          haveBannedRules = not (IM.null newStats) && any (isBanned @l @schd i) newStats
      if
          -- If we saturated because no rules matched and we have banned rules,
          -- unban them and try once more. This handles the case where all
          -- applicable rules got banned before they could finish their work.
         | saturated && noRulesMatched && haveBannedRules ->
             runEqualitySaturation' (i+1) mempty  -- Reset stats to unban all rules
          -- We have reached true saturation. We are done.
         | saturated -> return ()
          -- There's more to be done.
         | otherwise -> runEqualitySaturation' (i+1) newStats

  sameClass :: EClass a l -> EClass a l -> Bool
  sameClass before after = eClassData before == eClassData after
                       && eClassNodes before == eClassNodes after

  -- | Match a rewrite rule against the e-graph database.
  -- For conditional rewrites, we accumulate all conditions and filter matches
  -- by them BEFORE updating scheduler stats. This ensures the backoff scheduler
  -- only counts matches that would actually result in rewrites being applied.
  matchWithScheduler :: G.EGraph a l -> Database l -> Int -> IM.IntMap (Stat l schd) -> Int -> Rewrite a l
                     -> ([Match], IM.IntMap (Stat l schd), VarsState {- the vars mapping resulting from compiling the query -})
  matchWithScheduler egr db i stats rw_id rw = go [] rw
    where
      -- Accumulate conditions while recursing through conditional rewrites
      go :: [RewriteCondition a l] -> Rewrite a l -> ([Match], IM.IntMap (Stat l schd), VarsState)
      go conds (rw' :| cond) = go (cond:conds) rw'
      go conds base = doPattern conds (rewriteLhs base)

      doPattern conds lhs = do
          let (lhs_query, varsState) = compileToQuery lhs

          case IM.lookup rw_id stats of
            -- If it's banned until some iteration, don't match this rule
            -- against anything.
            Just s | isBanned @l @schd i s -> ([], stats, varsState)

            -- Otherwise, match and update stats
            x -> do

                -- Match pattern
                let matches' = ematch db lhs_query -- Add rewrite to the e-match substitutions

                -- Filter by all accumulated conditions BEFORE updating stats.
                -- This is critical: the backoff scheduler should only count
                -- matches where conditions actually pass, not all structural matches.
                let filteredMatches = case conds of
                      [] -> matches'  -- No conditions, use all matches
                      _  -> filter (matchSatisfiesConditions varsState conds) matches'

                -- Some scheduler: update stats on FILTERED matches
                let newStats = updateStats schd i rw_id rw x stats filteredMatches

                (filteredMatches, newStats, varsState)

      -- Check if a match satisfies ALL accumulated conditions
      matchSatisfiesConditions :: VarsState -> [RewriteCondition a l] -> Match -> Bool
      matchSatisfiesConditions vss conds (Match subst _) =
          all (\cond -> cond vss subst egr) conds

  applyMatchesRhs :: (Rewrite a l, Match, VarsState) -> EGraphM a l ()
  applyMatchesRhs (rw, Match subst eclass, vss) = do
      egr <- get
      forM_ (rewriteRhs rw vss subst egr) $ \rhs -> do
          eclass' <- reprPat vss subst rhs
          -- Preserve the representative choice of static variable RHSs.
          void $ case rhs of
              VariablePattern _ -> merge eclass' eclass
              NonVariablePattern _ -> merge eclass eclass'

  -- | Static and computed replacements share capture resolution and insertion.
  reprPat :: VarsState -> Subst -> Pattern l -> EGraphM a l ClassId
  reprPat vss subst = \case
      VariablePattern v -> gets $ G.find (findSubst (findVarName vss v) subst)
      NonVariablePattern p -> add . Node =<< traverse (reprPat vss subst) p
{-# INLINEABLE runEqualitySaturation #-}
