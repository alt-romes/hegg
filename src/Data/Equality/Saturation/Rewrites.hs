{-|

Definition of the 'Rewrite' datatype used to define rewrite rules that are to
be used with and applied during equality saturation.

-}
module Data.Equality.Saturation.Rewrites where

import Data.Equality.Graph
import Data.Equality.Matching

-- | A rewrite rule that might have conditions for being applied
--
-- === Example
-- @
-- rewrites :: [Rewrite Expr] -- from Sym.hs
-- rewrites =
--     [ "x"+"y" := "y"+"x" -- comm add
--     , "x"*("y"*"z") := ("x"*"y")*"z" -- assoc mul
--
--     , "x"/"y" := "x"*powP "y" (-1) :| is_not_zero "y" -- div cannon
--
--     , "x"*0 := 0
--     , "x"*1 := "x"
--
--     , "a"-"a" := 1 -- cancel sub
--     , "a"/"a" := 1 :| is_not_zero "a" -- cancel div
--
--     ]
-- @
data Rewrite lang = !(Pattern lang) := !(Pattern lang)
                  | !(Rewrite lang) :| !(RewriteCondition lang) -- Conditional rewrites
infix 3 :=
infixl 2 :|

-- | A rewrite condition. With a substitution from bound variables to e-classes
-- and with the e-graph, return true when a condition is satisfied
--
-- === Example
-- @
-- is_not_zero :: String -> RewriteCondition Expr
-- is_not_zero v subst egr =
--    case L.lookup v subst of
--      Just class_id ->
--          egr^._class class_id._data /= Just 0
-- @
type RewriteCondition lang = Subst -> EGraph lang -> Bool

