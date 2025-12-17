{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Data.Equality.Utils.Untyped
Description : Type erasure layer for indexed expressions
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides the type-erasing wrapper infrastructure that allows
type-indexed expressions to be stored in the untyped e-graph while
preserving type information for reconstruction.

The key insight is that 'ErasedLang l' satisfies the 'Language' constraint
when 'l' is a proper type-indexed functor, allowing reuse of the entire
existing e-graph infrastructure.

Example:

@
-- A type-indexed expression language
data TExpr (dom :: Ty) a where
    TConst :: Int -> TExpr 'TyInt a
    TAdd   :: a -> a -> TExpr 'TyInt a

-- Wrap in Untyped to hide type indices
wrapped :: Untyped TExpr ClassId
wrapped = Untyped (TAdd someClassId otherClassId)

-- ErasedLang provides Language instance for e-graph storage
erased :: ErasedLang TExpr ClassId
erased = ErasedLang wrapped
@
-}
module Data.Equality.Utils.Untyped
    ( -- * Type erasure
      Untyped(..)
    , UntypedWith(..)
    , ErasedLang(..)
      -- * Type recovery
    , withUntyped
    , matchType
    ) where

import Data.Kind (Type)
import Data.Type.Equality ()

import Data.Equality.Utils.Singleton

-- | Kind alias for type-indexed language functors.
--
-- A type-indexed language functor has the form @l :: k -> Type -> Type@
-- where:
--
-- * The first parameter @k@ is the domain type (result type)
-- * The second parameter @Type@ is the recursive position type
--
-- Note: We don't use a type synonym because GHC doesn't support poly-kinded
-- type synonyms well. Instead, we document the expected kind inline.

-- | Type-erasing wrapper that preserves singleton witnesses.
--
-- This GADT existentially hides the type index @dom@ while
-- keeping 'SingI' constraint that allows recovery of the
-- type information at runtime.
--
-- The parameter @l@ has kind @k -> Type -> Type@.
data Untyped l a where
    Untyped :: SingI dom
            => l dom a
            -> Untyped l a

-- | Type-erasing wrapper that also stores the singleton witness explicitly.
--
-- This variant preserves both explicit singleton value and implicit constraint,
-- useful when you need to compare or order Untyped values.
data UntypedWith l a where
    UntypedWith :: SingI dom
                => Sing dom -> l dom a -> UntypedWith l a

-- | Convert to the explicit form for comparison.
toUntypedWith :: Untyped l a -> UntypedWith l a
toUntypedWith (Untyped x) = UntypedWith sing x
{-# INLINE toUntypedWith #-}

-- | Eliminate an 'Untyped' value by providing a continuation.
withUntyped :: Untyped l a
            -> (forall dom. SingI dom => l dom a -> r)
            -> r
withUntyped (Untyped x) f = f x
{-# INLINE withUntyped #-}

-- | Attempt to recover a specific type from an 'Untyped' value.
--
-- Returns 'Just' the underlying expression if the type index matches,
-- 'Nothing' otherwise.
matchType :: forall k (dom :: k) l a.
             (SingI dom, SDecide k)
          => Sing dom -> Untyped l a -> Maybe (l dom a)
matchType expectedDom u =
    case toUntypedWith u of
        UntypedWith actualDom x ->
            case decEq expectedDom actualDom of
                Just Refl -> Just x
                _ -> Nothing
{-# INLINE matchType #-}

-- | Newtype wrapper for Language instance derivation.
--
-- 'ErasedLang l' satisfies the 'Language' constraint when @l@ has appropriate
-- instances, allowing type-indexed expressions to be used with the existing
-- e-graph infrastructure.
--
-- The parameter @l@ has kind @k -> Type -> Type@.
newtype ErasedLang l a =
    ErasedLang { getErased :: Untyped l a }

-- | Functor instance for ErasedLang using QuantifiedConstraints.
instance (forall dom. Functor (l dom)) => Functor (ErasedLang l) where
    fmap f (ErasedLang (Untyped x)) = ErasedLang (Untyped (fmap f x))
    {-# INLINE fmap #-}

-- | Foldable instance for ErasedLang.
instance (forall dom. Foldable (l dom)) => Foldable (ErasedLang l) where
    foldMap f (ErasedLang (Untyped x)) = foldMap f x
    {-# INLINE foldMap #-}

-- | Traversable instance for ErasedLang.
instance (forall dom. Traversable (l dom)) => Traversable (ErasedLang l) where
    traverse f (ErasedLang (Untyped x)) = ErasedLang . Untyped <$> traverse f x
    {-# INLINE traverse #-}

-- | Eq instance comparing type index first, then structure.
--
-- Two erased expressions are equal iff they have the same type index
-- AND the same structure.
--
-- Note: The type parameter @l@ has kind @k -> Type -> Type@.
instance (forall dom. SingI dom => Eq (l dom a), SDecide k)
         => Eq (ErasedLang (l :: k -> Type -> Type) a) where
    ErasedLang u1 == ErasedLang u2 =
        case (toUntypedWith u1, toUntypedWith u2) of
            (UntypedWith dom1 x, UntypedWith dom2 y) ->
                case decEq dom1 dom2 of
                    Just Refl -> x == y
                    _ -> False
    {-# INLINE (==) #-}

-- | Ord instance providing consistent total ordering.
--
-- Orders by type index first (using SOrd), then by structure.
-- This is needed for e-graph memo table storage.
--
-- Note: The type parameter @l@ has kind @k -> Type -> Type@.
instance ( forall dom. SingI dom => Eq (l dom a)
         , forall dom. SingI dom => Ord (l dom a)
         , SOrd k
         )
         => Ord (ErasedLang (l :: k -> Type -> Type) a) where
    compare (ErasedLang u1) (ErasedLang u2) =
        case (toUntypedWith u1, toUntypedWith u2) of
            (UntypedWith dom1 x, UntypedWith dom2 y) ->
                case sCompare dom1 dom2 of
                    SLT -> LT
                    SGT -> GT
                    SEQ -> case decEq dom1 dom2 of
                        Just Refl -> compare x y
                        -- This case is impossible since sCompare returned SEQ
                        _ -> EQ
    {-# INLINE compare #-}

-- | Show instance for debugging.
instance (forall dom. SingI dom => Show (l dom a))
         => Show (ErasedLang l a) where
    showsPrec d (ErasedLang (Untyped x)) = showParen (d > 10) $
        showString "ErasedLang " . showsPrec 11 x
