{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-|
   This module defines 'IntToIntMap', a variant of 'Data.IntMap' in which the
   values are fixed to 'Int'.

   We make use of this structure in 'Data.Equality.Graph.ReprUnionFind' to
   improve performance by a constant factor
 -}
module Data.Equality.Utils.IntToIntMap
  ( IntToIntMap(Nil)
  , Key, Val
  , find, insert, (!)
  , unliftedFoldr
  ) where

import GHC.Exts
import Data.Bits

-- | A map of integers to integers
type IntToIntMap :: TYPE ('BoxedRep 'Unlifted)
data IntToIntMap = Bin Prefix Mask IntToIntMap IntToIntMap
                 | Tip InternalKey Val
                 | Nil -- ^ An empty 'IntToIntMap'. Ideally this would be defined as a function instead of an exported constructor, but it's currently not possible to have top-level bindings for unlifted datatypes

type Prefix      = Word#
type Mask        = Word#
type InternalKey = Word#

-- | Key type synonym in an 'IntToIntMap'
type Key         = Int#
-- | Value type synonym in an 'IntToIntMap'
type Val         = Int#

-- | \(O(\min(n,W))\). Find the value at a key.
-- Calls 'error' when the element can not be found.
(!) :: IntToIntMap -> Key -> Val
(!) m k = find k m
{-# INLINE (!) #-}

-- | Find the 'Val' for a 'Key' in an 'IntToIntMap'
find :: Key -> IntToIntMap -> Val
find (int2Word# -> k) = find' k
{-# INLINE find #-}

-- | Insert a 'Val' at a 'Key' in an 'IntToIntMap'
insert :: Key -> Val -> IntToIntMap -> IntToIntMap
insert k = insert' (int2Word# k)
{-# INLINE insert #-}

insert' :: InternalKey -> Val -> IntToIntMap -> IntToIntMap
insert' k x t@(Bin p m l r)
  | nomatch k p m = link k (Tip k x) p t
  | zero k m      = Bin p m (insert' k x l) r
  | otherwise     = Bin p m l (insert' k x r)
insert' k x t@(Tip ky _)
  | isTrue# (k `eqWord#` ky) = Tip ky x
  | otherwise                = link k (Tip k x) ky t
insert' k x Nil = Tip k x

-- DANGEROUS NOTE:
-- Since this is the function that currently takes 10% of runtime, we want to
-- improve constant factors: we'll remove the comparison that checks that the
-- tip we found is the tip we are looking for. This is a very custom map,
-- we will assume the tip we find is ALWAYS the one we are looking for. This,
-- of course, will return wrong results instead of blow up if we use it
-- unexpectedly. Hopefully the testsuite will serve to warn us of this
--
-- Update: The speedup is not noticeable, so we don't do it, but I'll leave the comment here for now
find' :: InternalKey -> IntToIntMap -> Val
find' k (Bin _p m l r)
  | zero k m  = find' k l
  | otherwise = find' k r
find' k (Tip kx x) | isTrue# (k `eqWord#` kx) = x
find' _ _ = error ("IntMap.!: key ___ is not an element of the map")

-- * Other stuff taken from IntMap

link :: Prefix -> IntToIntMap -> Prefix -> IntToIntMap -> IntToIntMap
link p1 t1 p2 t2 = linkWithMask (highestBitMask (p1 `xor#` p2)) p1 t1 {-p2-} t2
{-# INLINE link #-}

-- `linkWithMask` is useful when the `branchMask` has already been computed
linkWithMask :: Mask -> Prefix -> IntToIntMap -> IntToIntMap -> IntToIntMap
linkWithMask m p1 t1 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    p = maskW p1 m
{-# INLINE linkWithMask #-}


-- The highestBitMask implementation is based on
-- http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
-- which has been put in the public domain.

-- | Return a word where only the highest bit is set.
highestBitMask :: Word# -> Word#
highestBitMask w =
  case finiteBitSize (0 :: Word) of
    I# wordSize -> shiftL# (int2Word# 1#) (wordSize -# 1# -# (word2Int# (clz# w)))
{-# INLINE highestBitMask #-}

nomatch :: InternalKey -> Prefix -> Mask -> Bool
nomatch i p m
  = isTrue# ((maskW i m) `neWord#` p)
{-# INLINE nomatch #-}

-- | The prefix of key @i@ up to (but not including) the switching
-- bit @m@.
maskW :: Word# -> Word# -> Prefix
maskW i m
  = (i `and#` ((int2Word# (negateInt# (word2Int# m))) `xor#` m))
{-# INLINE maskW #-}

zero :: InternalKey -> Mask -> Bool
zero i m
  = isTrue# ((i `and#` m) `eqWord#` (int2Word# 0#))
{-# INLINE zero #-}

-- | A 'foldr' in which the accumulator is unlifted
unliftedFoldr :: forall a {b :: TYPE ('BoxedRep 'Unlifted)} . (a -> b -> b) -> b -> [a] -> b 
unliftedFoldr k z = go
  where
    go []     = z
    go (y:ys) = y `k` go ys
