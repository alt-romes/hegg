{-# LANGUAGE FlexibleContexts #-}
{-|

Defines 'Language', which is the required constraint for most e-graph and
equality saturation operations.

A 'Language' is the form of the data manipulated by an e-graph.

=== Example
@
data Expr a = Sym String
            | Const Double
            | UnOp  UOp a
            | BinOp BOp a a
            deriving ( Eq, Ord, Functor
                     , Foldable, Traversable)

instance Eq1 Expr  where
    ...
instance Ord1 Expr where
    ...

instance Analysis Expr where
    ...

-- meaning we satisfy all other constraints and Expr is! a language
instance Language Expr

@
-}
module Data.Equality.Language
    ( module Data.Equality.Language
    , module Data.Equality.Analysis
    ) where

import Data.Functor.Classes

import Data.Equality.Analysis

-- | A language is a recursive data type written in its functor \"form\"
--
-- Must satisfy all other class constraints
class (Analysis l, Traversable l, Ord1 l) => Language l where

