{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Data.Equality.Language.Indexed
Description : Language constraint for type-indexed expressions
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides the 'LanguageI' constraint for type-indexed expression
languages, analogous to the 'Language' constraint but for languages with
an additional type index parameter.

A type-indexed language functor has the form @l :: k -> Type -> Type@
where:

* The first parameter @k@ is the domain type (result type)
* The second parameter @Type@ is the recursive position type

Example:

@
data TExpr (dom :: Ty) a where
    TConst :: Int -> TExpr 'TyInt a
    TAdd   :: a -> a -> TExpr 'TyInt a

-- TExpr satisfies LanguageI when it has:
-- - Functor (TExpr dom) for all dom
-- - Foldable (TExpr dom) for all dom
-- - Traversable (TExpr dom) for all dom
-- - Ord (TExpr dom a) for all dom and Ord a
@
-}
module Data.Equality.Language.Indexed
    ( -- * Language constraint for indexed languages
      LanguageI
      -- * Constraint dictionary
    , Dict(..)
      -- * Proof of Language constraint for ErasedLang
    , erasedIsLanguage
    ) where

import Data.Kind (Type, Constraint)

import Data.Equality.Language
import Data.Equality.Utils.Singleton
import Data.Equality.Utils.Untyped

-- | Constraint dictionary. Allows reifying a constraint into a value
-- that can be passed around and pattern matched on to bring the
-- constraint back into scope.
data Dict (c :: Constraint) where
    Dict :: c => Dict c

-- | A 'LanguageI' is the required constraint on type-indexed expressions
-- that are to be represented in an e-graph.
--
-- This mirrors the 'Language' constraint but for type-indexed languages
-- with kind @k -> Type -> Type@.
--
-- For a type-indexed language to be usable with e-graphs:
--
-- 1. It must be 'Traversable' for all type index instantiations
-- 2. It must have 'Ord' for all type index instantiations when the
--    recursive position has 'Ord'
--
-- These constraints ensure that 'ErasedLang l' satisfies the regular
-- 'Language' constraint.
type LanguageI :: forall k. (k -> Type -> Type) -> Constraint
class ( forall dom. Traversable (l dom)
      , forall dom a. (SingI dom, Ord a) => Ord (l dom a)
      ) => LanguageI l

instance ( forall dom. Traversable (l dom)
         , forall dom a. (SingI dom, Ord a) => Ord (l dom a)
         ) => LanguageI l

-- | Proof that 'ErasedLang l' satisfies the 'Language' constraint whenever
-- @l@ satisfies 'LanguageI' and the kind @k@ has decidable equality and
-- ordering.
--
-- This function returns a constraint dictionary that can be pattern matched
-- on to bring the 'Language (ErasedLang l)' constraint into scope.
--
-- Example usage:
--
-- @
-- case erasedIsLanguage @MyKind @MyLang of
--     Dict -> -- Here, Language (ErasedLang MyLang) is in scope
--         equalitySaturation (getEGraphI egraph) ...
-- @
erasedIsLanguage :: forall k (l :: k -> Type -> Type).
                    (LanguageI l, SOrd k)
                 => Dict (Language (ErasedLang l))
erasedIsLanguage = Dict
{-# INLINE erasedIsLanguage #-}
