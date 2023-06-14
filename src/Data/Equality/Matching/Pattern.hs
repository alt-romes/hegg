{-# LANGUAGE QuantifiedConstraints, RankNTypes, UnicodeSyntax, UndecidableInstances #-}
{-|
   Definition of 'Pattern' for use in equality matching
   ('Data.Equality.Matching'), where patterns are matched against the e-graph
 -}
module Data.Equality.Matching.Pattern where

import Data.String

import Data.Equality.Utils
import Data.Equality.Matching.Database

-- | A pattern can be either a variable or an non-variable expression of
-- patterns.
--
-- A 'NonVariablePattern' will only match an expression if the @lang@ constructor matches an expression and all child patterns match the expression children.
-- A 'VariablePattern' matches any expression.
--
-- === Example
--
-- The expression
--
-- @
-- expr :: Fix Sym
-- expr = BinOp Add (Sym "x") (Const 2.0) -- i.e. x + 2
-- @
--
-- Would be matched against the following patterns
--
-- @
-- pat1 :: Pattern Sym
-- pat1 = VariablePattern 1
--
-- pat2 :: Pattern Sym
-- pat2 = NonVariablePattern (BinOp Add (VariablePattern 1) (VariablePattern 2))
--
-- pat3 :: Pattern Sym
-- pat3 = NonVariablePattern (BinOp Add (VariablePattern 1) (NonVariablePattern (Const 2)))
-- @
--
-- But would not be matched against the following patterns
-- 
-- @
-- pat4 :: Pattern Sym
-- pat4 = NonVariablePattern (Const 5)
--
-- pat5 :: Pattern Sym
-- pat5 = NonVariablePattern (BinOp Add (NonVariablePattern (Sym "y")) (NonVariablePattern (Const 2)))
--
-- pat6 :: Pattern Sym
-- pat6 = NonVariablePattern (BinOp Add (NonVariablePattern (Sym "x")) (NonVariablePattern (Const 3)))
-- @
--
-- === IsString
-- 'Pattern' instances 'IsString', which means one can write a variable pattern simply as a string.
--
-- It works by using 'Data.Equality.Utils.hashString' to create a unique integer for a 'VariablePattern'
--
-- For example, we could write the following pattern that would match @a+a@ and @b+b@ but not @a+b@
--
-- @
-- pat7 :: Pattern Sym
-- pat7 = 'pat' (BinOp Add "x" "x")
-- @
data Pattern lang
    = NonVariablePattern (lang (Pattern lang))
    | VariablePattern Var -- ^ Should be a >0 positive number

-- | Synonym for 'NonVariablePattern'.
--
-- Example
--
-- @
-- pat8 :: Pattern Sym
-- pat8 = pat (BinOp Mul "y" (pat (Const 2))) -- matches any product of an expression by 2
-- @
pat :: lang (Pattern lang) -> Pattern lang
pat = NonVariablePattern

instance (∀ a. Eq a => Eq (l a)) => (Eq (Pattern l)) where
    (==) (NonVariablePattern a) (NonVariablePattern b) = (==) a b
    (==) (VariablePattern a) (VariablePattern b) = a == b 
    (==) _ _ = False

instance (∀ a. Eq a => Eq (l a), ∀ a. (Ord a) => Ord (l a)) => (Ord (Pattern l)) where
    compare (VariablePattern _) (NonVariablePattern _) = LT
    compare (NonVariablePattern _) (VariablePattern _) = GT
    compare (VariablePattern a) (VariablePattern b) = compare a b
    compare (NonVariablePattern a) (NonVariablePattern b) = compare a b

instance (∀ a. Show a => Show (lang a)) => Show (Pattern lang) where
    showsPrec _ (VariablePattern s) = showString (show s) -- ROMES:TODO don't ignore prec?
    showsPrec d (NonVariablePattern x) = showsPrec d x

instance IsString (Pattern lang) where
    fromString = VariablePattern . hashString

