{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Data.Equality.Utils.HList
Description : Heterogeneous list infrastructure
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides heterogeneous list infrastructure for representing lists
of typed values, needed for function argument lists in type-indexed expressions.

Example usage:

@
-- A list of terms with different types
type Args = HList (Term env) '[TyInt, TyBool, TyString]

-- Constructing an HList
args :: Args
args = intTerm `HCons` boolTerm `HCons` stringTerm `HCons` HNil
@
-}
module Data.Equality.Utils.HList
    ( -- * Heterogeneous lists
      HList(..)
      -- * Membership proofs
    , In(..)
      -- * Operations
    , hLookup
    , hMap
    , hFoldr
    , hLength
      -- * Type-level length
    , Length
    ) where

import Data.Kind (Type)
import GHC.TypeLits (Nat, type (+))

-- | Heterogeneous list indexed by a type-level list.
--
-- The @f@ parameter is a type constructor that wraps each element type,
-- and @ts@ is the type-level list of element types.
--
-- For example, @HList Identity '[Int, Bool, String]@ contains an 'Int',
-- a 'Bool', and a 'String'.
data HList (f :: k -> Type) (ts :: [k]) where
    HNil  :: HList f '[]
    HCons :: f t -> HList f ts -> HList f (t ': ts)

infixr 5 `HCons`

-- | Type-safe proof that a type @t@ is a member of the type-level list @ts@.
--
-- This GADT acts as a type-safe index into an HList. 'Here' indicates the
-- element is at the head, while 'There' indicates it's further in the list.
data In (t :: k) (ts :: [k]) where
    Here  :: In t (t ': ts)
    There :: In t ts -> In t (t' ': ts)

-- | Lookup an element in an HList by its membership proof.
--
-- This is guaranteed to succeed because the 'In' proof ensures the
-- element exists at the correct position.
hLookup :: In t ts -> HList f ts -> f t
hLookup Here      (HCons x _)  = x
hLookup (There i) (HCons _ xs) = hLookup i xs
{-# INLINE hLookup #-}

-- | Map a natural transformation over all elements of an HList.
--
-- This preserves the type-level list structure while transforming
-- each element from @f t@ to @g t@.
hMap :: (forall t. f t -> g t) -> HList f ts -> HList g ts
hMap _ HNil = HNil
hMap f (HCons x xs) = HCons (f x) (hMap f xs)
{-# INLINE hMap #-}

-- | Right fold over an HList.
--
-- This allows uniform processing of all elements, discarding the
-- type information at the result.
hFoldr :: (forall t. f t -> b -> b) -> b -> HList f ts -> b
hFoldr _ z HNil = z
hFoldr f z (HCons x xs) = f x (hFoldr f z xs)
{-# INLINE hFoldr #-}

-- | Get the length of an HList at the term level.
hLength :: HList f ts -> Int
hLength = hFoldr (\_ acc -> acc + 1) 0
{-# INLINE hLength #-}

-- | Compute the length of a type-level list.
type family Length (ts :: [k]) :: Nat where
    Length '[] = 0
    Length (_ ': ts) = 1 + Length ts

-- | Show instance for empty HList.
instance Show (HList f '[]) where
    show HNil = "HNil"

-- | Show instance for non-empty HList.
instance (Show (f t), Show (HList f ts)) => Show (HList f (t ': ts)) where
    showsPrec d (HCons x xs) = showParen (d > 5) $
        showsPrec 6 x . showString " `HCons` " . showsPrec 5 xs

-- | Eq instance for empty HList.
instance Eq (HList f '[]) where
    HNil == HNil = True
    {-# INLINE (==) #-}

-- | Eq instance for non-empty HList.
instance (Eq (f t), Eq (HList f ts)) => Eq (HList f (t ': ts)) where
    HCons x xs == HCons y ys = x == y && xs == ys
    {-# INLINE (==) #-}

-- | Ord instance for empty HList.
instance Ord (HList f '[]) where
    compare HNil HNil = EQ
    {-# INLINE compare #-}

-- | Ord instance for non-empty HList.
instance (Ord (f t), Ord (HList f ts)) => Ord (HList f (t ': ts)) where
    compare (HCons x xs) (HCons y ys) = case compare x y of
        EQ -> compare xs ys
        r  -> r
    {-# INLINE compare #-}

-- | Functor-like mapping when the HList parameter is a functor.
-- Note: HList itself is not a Functor because it doesn't have kind Type -> Type.

-- | Foldable-like operations.
-- Note: HList itself is not Foldable for the same reason.

-- | Traversable-like operations.
-- Note: HList itself is not Traversable for the same reason.
