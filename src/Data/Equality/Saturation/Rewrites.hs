{-# LANGUAGE QuantifiedConstraints, RankNTypes, UnicodeSyntax #-}
{-|

Definition of 'Rewrite' and 'RewriteCondition' used to define rewrite rules.

Rewrite rules are applied to all represented expressions in an e-graph every
iteration of equality saturation.

-}
module Data.Equality.Saturation.Rewrites where

import Data.Equality.Graph
import Data.Equality.Matching
import Data.Equality.Matching.Database

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
data Rewrite anl lang = !(Pattern lang) := !(Pattern lang)          -- ^ Trivial Rewrite
                      | !(Rewrite anl lang) :| !(RewriteCondition anl lang) -- ^ Conditional Rewrite
infix 3 :=
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
type RewriteCondition anl lang = Subst -> EGraph anl lang -> Bool


instance (∀ a. Show a => Show (lang a)) => Show (Rewrite anl lang) where
  show (rw :| _) = show rw <> " :| <cond>"
  show (lhs := rhs) = show lhs <> " := " <> show rhs
