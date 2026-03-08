{-# LANGUAGE QuantifiedConstraints, RankNTypes, UnicodeSyntax #-}
{-|

Definition of 'Rewrite' and 'RewriteCondition' used to define rewrite rules.

Rewrite rules are applied to all represented expressions in an e-graph every
iteration of equality saturation.

-}
module Data.Equality.Saturation.Rewrites
    ( Rewrite(..)
    , RewriteCondition
    , RewriteFun
    , MatchInfo(..)
    ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import Data.Equality.Graph
import Data.Equality.Matching
import Data.Equality.Matching.Database
import Data.Equality.Utils (Fix)

-- | A rewrite rule that might have conditions for being applied
--
-- === __Example__
-- @
-- rewrites :: [Rewrite Expr] -- from Sym.hs
-- rewrites =
--     [ "x"+"y" := "y"+"x"
--     , "x"*("y"*"z") := ("x"*"y")*"z"
--
--     , "x"*0 := 0
--     , "x"*1 := "x"
--
--     , "a"-"a" := 1 -- cancel sub
--     , "a"/"a" := 1 :| is_not_zero "a"
--     ]
-- @
--
-- See the definition of @is_not_zero@ in the documentation for
-- 'RewriteCondition'
--
-- === __Soundness__
--
-- Conditional rewrites (':|') and computed rewrites (':=>') must satisfy
-- a /monotonicity/ property to ensure confluence:
--
-- Given expressions \(e_0\) and \(e_1\) matching the rule's LHS where
-- \(\mathit{analysis}(e_0) \sqsubseteq \mathit{analysis}(e_1)\) (with respect
-- to the analysis join semilattice), if the rewrite fires for \(e_0\), it
-- must also fire for \(e_1\).
--
-- In other words, learning more information (via the analysis) should never
-- cause a rewrite to stop firing.
--
data Rewrite anl lang
    = !(Pattern lang) := !(Pattern lang)
    -- ^ Trivial Rewrite
    | !(Pattern lang) :=> (RewriteFun anl lang)
    -- ^ A computed rewrite. The RHS is computed by a function that receives
    -- a 'MatchInfo' for each pattern variable, providing access to the
    -- analysis data and e-nodes in the matched e-classes.
    --
    -- The function returns 'Just' with the computed expression to add to the
    -- e-graph, or 'Nothing' to skip this match.
    --
    -- === __Example: Constant Folding__
    -- @
    -- foldAdd :: Rewrite () Lang
    -- foldAdd = pat (x \`Add\` y) :=> \\ctx -> do
    --     MatchInfo _ xNodes <- Map.lookup "x" ctx
    --     MatchInfo _ yNodes <- Map.lookup "y" ctx
    --     Node (Lit xn) <- findLit xNodes
    --     Node (Lit yn) <- findLit yNodes
    --     pure $ Fix $ Lit (xn + yn)
    -- @
    --
    -- The 'MatchInfo' interface ensures computed rewrites can only inspect
    -- the matched e-classes, not arbitrary e-graph structure. This makes it
    -- easier to write confluent rewrites.
    | !(Rewrite anl lang) :| !(RewriteCondition anl lang)
    -- ^ Conditional Rewrite
infix 3 :=
infixl 3 :=>
infixl 2 :|

-- | A rewrite condition. With a substitution from bound variables in the
-- pattern to e-classes and with the e-graph, return 'True' if the condition is
-- satisfied
--
-- === Example
-- @
-- is_not_zero :: String -> RewriteCondition Expr
-- is_not_zero v subst egr =
--    case lookup v subst of
--      Just class_id ->
--          egr^._class class_id._data /= Just 0
-- @
type RewriteCondition anl lang = VarsState -> Subst -> EGraph anl lang -> Bool

-- | Information about a matched pattern variable, providing access to the
-- analysis data and nodes in the matched e-class.
--
-- This is the restricted interface provided to 'RewriteFun', ensuring that
-- computed rewrites can only inspect matched classes rather than arbitrary
-- e-graph structure.
data MatchInfo anl lang = MatchInfo
    { matchAnalysis :: anl
      -- ^ The analysis data for the matched e-class
    , matchNodes    :: Set (ENode lang)
      -- ^ The e-nodes in the matched e-class
    }

-- | A function to compute the RHS of a rewrite.
--
-- The function receives a 'Map' from pattern variable names to 'MatchInfo',
-- containing the analysis data and nodes for each matched e-class.
--
-- Return 'Just' with the computed expression to add it to the e-graph and
-- merge it with the matched e-class, or 'Nothing' to skip this match.
--
-- __Soundness requirement__: The returned expression must be semantically
-- equivalent to the expression matched by the LHS pattern.
--
-- __Confluence requirement__: The result should be deterministic — it should
-- depend only on the semantic content of the matched classes (analysis data,
-- node values), not on incidental details like iteration order over sets.
type RewriteFun anl lang = Map String (MatchInfo anl lang) -> Maybe (Fix lang)


instance (∀ a. Show a => Show (lang a)) => Show (Rewrite anl lang) where
  show (rw :| _) = show rw <> " :| <cond>"
  show (lhs := rhs) = show lhs <> " := " <> show rhs
  show (lhs :=> _) = show lhs <> " :=> <fun>"
