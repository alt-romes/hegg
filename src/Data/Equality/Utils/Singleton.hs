{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-|
Module      : Data.Equality.Utils.Singleton
Description : Singleton infrastructure for runtime type information
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides foundational singleton infrastructure for runtime type
information, enabling type-safe erasure and recovery of type indices.

Users can provide their own 'Sing' instances (via Template Haskell with
singletons-th, or manually) to use type-indexed expressions with hegg.

Example:

@
data Ty = TyInt | TyBool | TyFunc Ty Ty

data instance Sing (t :: Ty) where
    STyInt  :: Sing 'TyInt
    STyBool :: Sing 'TyBool
    STyFunc :: Sing a -> Sing b -> Sing ('TyFunc a b)

instance SingI 'TyInt where sing = STyInt
instance SingI 'TyBool where sing = STyBool
instance (SingI a, SingI b) => SingI ('TyFunc a b) where
    sing = STyFunc sing sing
@
-}
module Data.Equality.Utils.Singleton
    ( -- * Core singleton types
      Sing
    , SingI(..)
      -- * Type equality decisions
    , SDecide(..)
      -- * Ordering on singletons
    , SOrd(..)
    , SOrdering(..)
      -- * Existential wrappers
    , SomeType(..)
    , withSomeType
    , fromSomeType
      -- * List-kinded singletons
    , SingList(..)
    , SingListI(..)
    , sListLength
      -- * List-level equality and ordering
    , decEqList
    , sCompareList
      -- * Type equality
    , (:~:)(..)
    ) where

import Data.Kind (Type, Constraint)
import Data.Type.Equality ((:~:)(..))

-- | Data family for singletons. Users provide instances for their own kinds.
--
-- A singleton is a type with exactly one inhabitant for each value of the
-- indexed type. This allows runtime access to type-level information.
type Sing :: k -> Type
data family Sing (t :: k)

-- | Typeclass for implicit singleton access.
--
-- Provides a way to obtain a singleton value when the type is known.
type SingI :: forall k. k -> Constraint
class SingI (t :: k) where
    sing :: Sing t

-- | Decision procedure for type equality.
--
-- Given two singleton values, decide whether the underlying types are equal.
-- Returns 'Just Refl' if equal, 'Nothing' otherwise.
--
-- Users must implement this class for their custom kinds.
class SDecide k where
    decEq :: forall (a :: k) (b :: k). Sing a -> Sing b -> Maybe (a :~: b)

-- | Ordering result at the type level.
data SOrdering = SLT | SEQ | SGT
    deriving (Eq, Ord, Show)

-- | Ordering on singleton values.
--
-- Provides a total ordering on types of a given kind via their singletons.
-- This is needed for storing type-erased values in ordered containers.
class SDecide k => SOrd k where
    sCompare :: forall (a :: k) (b :: k). Sing a -> Sing b -> SOrdering

-- | Existential wrapper for types with singletons.
--
-- Hides the type index while preserving the ability to recover it at runtime.
data SomeType k where
    SomeType :: SingI t => Sing (t :: k) -> SomeType k

-- | Eliminate a 'SomeType' by providing a continuation.
withSomeType :: SomeType k -> (forall (t :: k). SingI t => Sing t -> r) -> r
withSomeType (SomeType s) f = f s
{-# INLINE withSomeType #-}

-- | Attempt to recover a specific type from a 'SomeType'.
--
-- Returns 'Just' if the hidden type matches the expected type,
-- 'Nothing' otherwise.
fromSomeType :: forall k (t :: k). (SingI t, SDecide k)
             => Sing t -> SomeType k -> Maybe (Sing t)
fromSomeType expected (SomeType actual) = case decEq actual expected of
    Just Refl -> Just actual
    Nothing   -> Nothing
{-# INLINE fromSomeType #-}

-- | Eq instance for SomeType using SDecide.
instance SDecide k => Eq (SomeType k) where
    SomeType a == SomeType b = case decEq a b of
        Just Refl -> True
        Nothing   -> False
    {-# INLINE (==) #-}

-- | Ord instance for SomeType using SOrd.
instance SOrd k => Ord (SomeType k) where
    compare (SomeType a) (SomeType b) = case sCompare a b of
        SLT -> LT
        SEQ -> EQ
        SGT -> GT
    {-# INLINE compare #-}

-- | Singleton for type-level lists.
--
-- This is a GADT that mirrors the structure of type-level lists at the term level.
data SingList (ts :: [k]) where
    SNil  :: SingList '[]
    SCons :: Sing t -> SingList ts -> SingList (t ': ts)

infixr 5 `SCons`

-- | Typeclass for implicit list singleton access.
type SingListI :: forall k. [k] -> Constraint
class SingListI (ts :: [k]) where
    singList :: SingList ts

instance SingListI '[] where
    singList = SNil
    {-# INLINE singList #-}

instance (SingI t, SingListI ts) => SingListI (t ': ts) where
    singList = SCons sing singList
    {-# INLINE singList #-}

-- | Get the length of a singleton list.
sListLength :: SingList ts -> Int
sListLength sl = go sl
  where
    go :: SingList xs -> Int
    go SNil = 0
    go (SCons _ rest) = 1 + go rest
{-# INLINE sListLength #-}

-- | Show instance for SingList.
instance Show (SingList '[]) where
    show SNil = "SNil"

instance (Show (Sing t), Show (SingList ts)) => Show (SingList (t ': ts)) where
    showsPrec d (SCons x xs) = showParen (d > 5) $
        showsPrec 6 x . showString " `SCons` " . showsPrec 5 xs

-- | Decision procedure for type-level list equality.
decEqList :: forall k (as :: [k]) (bs :: [k]). SDecide k
          => SingList as -> SingList bs -> Maybe (as :~: bs)
decEqList SNil SNil = Just Refl
decEqList (SCons a as) (SCons b bs) = do
    Refl <- decEq a b
    Refl <- decEqList as bs
    Just Refl
decEqList _ _ = Nothing
{-# INLINE decEqList #-}

-- | Ordering for type-level lists.
sCompareList :: forall k (as :: [k]) (bs :: [k]). SOrd k
             => SingList as -> SingList bs -> SOrdering
sCompareList SNil SNil = SEQ
sCompareList SNil (SCons _ _) = SLT
sCompareList (SCons _ _) SNil = SGT
sCompareList (SCons a as) (SCons b bs) = case sCompare a b of
    SLT -> SLT
    SGT -> SGT
    SEQ -> sCompareList as bs
{-# INLINE sCompareList #-}
