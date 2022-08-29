{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-|
   Util: A list with a O(1) size function
 -}
module Data.Equality.Utils.SizedList where

import qualified Data.List
import GHC.Exts
import Data.Foldable

-- | A list with O(1) size access and O(1) conversion to normal list
data SList a = SList ![a] {-# UNPACK #-} !Int
  deriving Traversable

instance Semigroup (SList a) where
  (<>) (SList a i) (SList b j) = SList (a <> b) (i+j)
  {-# INLINE (<>) #-}

instance Monoid (SList a) where
  mempty = SList mempty 0
  {-# INLINE mempty #-}

instance Functor SList where
  fmap f (SList a i) = SList (fmap f a) i
  {-# INLINE fmap #-}

instance Foldable SList where
  fold       ( SList l _) = fold l
  foldMap f  ( SList l _) = foldMap f l
  foldMap' f ( SList l _) = foldMap' f l
  foldr f b  ( SList l _) = foldr f b l
  foldr' f b ( SList l _) = foldr' f b l
  foldl f b  ( SList l _) = foldl f b l
  foldl' f b ( SList l _) = foldl' f b l
  foldr1 f   ( SList l _) = foldr1 f l
  foldl1 f   ( SList l _) = foldl1 f l
  toList     ( SList l _) = l
  null       ( SList l _) = Data.List.null l
  length     ( SList _ i) = i
  elem x     ( SList l _) = x `elem` l
  maximum    ( SList l _) = maximum l
  minimum    ( SList l _) = minimum l
  sum        ( SList l _) = sum l
  product    ( SList l _) = product l

instance IsList (SList a) where
  type Item (SList a) = a
  fromList l          = SList l (length l)
  fromListN i l       = SList l i
  toList (SList l _)  = l

-- | Prepend an item to the list in O(1)
(|:) :: a -> SList a -> SList a
(|:) a (SList l i) = SList (a:l) (i+1)
{-# INLINE (|:) #-}

-- | Make a normal list from the sized list in O(1)
toListSL :: SList a -> [a]
toListSL (SList l _) = l
{-# INLINE toListSL #-}

-- | Get the size of the list in O(1)
sizeSL :: SList a -> Int
sizeSL (SList _ i) = i
{-# INLINE sizeSL #-}
