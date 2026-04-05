{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
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

instance Language Expr

-- For a language with associative-commutative operators, override
-- 'normalizeNode' to sort children:
--
-- @
-- data ACExpr a = Var String | Add [a] | Mul [a]
--   deriving (Eq, Ord, Functor, Foldable, Traversable)
--
-- instance Language ACExpr where
--     normalizeNode (Add xs) = Add (sort xs)
--     normalizeNode (Mul xs) = Mul (sort xs)
--     normalizeNode other    = other
-- @

instance Analysis Expr where
    ...

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
--
-- For languages with associative-commutative operators, override
-- 'normalizeNode' to sort children of those operators. This gives multiset
-- canonicalization through the standard @Ord@-based memo map, avoiding the
-- need for commutativity\/associativity rewrite rules (which cause e-graph
-- size blowup).
type Language :: (Type -> Type) -> Constraint
class (∀ a. Ord a => Ord (l a), Traversable l) => Language l where

    -- | Normalize an e-node after canonicalization. Called after children are
    -- remapped to canonical class IDs via @fmap find@. Use this to sort
    -- children of associative-commutative operators, giving multiset
    -- equality through the standard @Ord@-based memo map.
    --
    -- Must be idempotent: @normalizeNode . normalizeNode = normalizeNode@
    --
    -- Default: identity (no normalization).
    normalizeNode :: Ord a => l a -> l a
    normalizeNode = id
    {-# INLINE normalizeNode #-}

