{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|

Defines 'Language', which is the required constraint on /expressions/ that are
to be represented in e-graph and on which equality saturation can be run.

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
module Data.Equality.Language where

import Data.Kind

-- | A 'Language' is the required constraint on /expressions/ that are to be
-- represented in an e-graph.
--
-- Recursive data types must be expressed in its functor form to instance
-- 'Language'. Additionally, for a datatype to be a 'Language' (used in
-- e-graphs), note that it must satisfy the other class constraints. In
-- particular an 'Data.Equality.Analysis.Analysis' must be defined for the
-- language.
type Language :: (Type -> Type) -> Constraint
class (∀ a. Ord a => Ord (l a), Traversable l) => Language l
instance (∀ a. Ord a => Ord (l a), Traversable l) => Language l

