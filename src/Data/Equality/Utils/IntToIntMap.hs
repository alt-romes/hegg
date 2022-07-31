{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Data.Equality.Utils.IntToIntMap where

import GHC.Exts
import Data.Bits

-- | A map of integers to integers
type IntToIntMap :: TYPE ('BoxedRep 'Unlifted)
data IntToIntMap = Bin Prefix Mask IntToIntMap IntToIntMap
                 | Tip InternalKey Val
                 | Nil

type Prefix      = Word#
type Mask        = Word#
type InternalKey = Word#
type Key         = Int#
type Val         = Int#

insert :: Key -> Val -> IntToIntMap -> IntToIntMap
insert k = insert' (int2Word# k)
{-# INLINE insert #-}

insert' :: InternalKey -> Val -> IntToIntMap -> IntToIntMap
insert' k x t@(Bin p m l r)
  | nomatch k p m = link k (Tip k x) p t
  | zero k m      = Bin p m (insert' k x l) r
  | otherwise     = Bin p m l (insert' k x r)
insert' k x t@(Tip ky _)
  | isTrue# (k `eqWord#` ky) = Tip k x
  | otherwise                = link k (Tip k x) ky t
insert' k x Nil = Tip k x

link :: Prefix -> IntToIntMap -> Prefix -> IntToIntMap -> IntToIntMap
link p1 t1 p2 t2 = linkWithMask (branchMask p1 p2) p1 t1 {-p2-} t2
{-# INLINE link #-}

-- `linkWithMask` is useful when the `branchMask` has already been computed
linkWithMask :: Mask -> Prefix -> IntToIntMap -> IntToIntMap -> IntToIntMap
linkWithMask m p1 t1 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    p = maskW p1 m
{-# INLINE linkWithMask #-}

-- | The first switching bit where the two prefixes disagree.
branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = highestBitMask (p1 `xor#` p2)
{-# INLINE branchMask #-}

-- The highestBitMask implementation is based on
-- http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
-- which has been put in the public domain.

-- | Return a word where only the highest bit is set.
highestBitMask :: Word# -> Word#
highestBitMask w =
  case finiteBitSize (0 :: Word) of
    I# wordSize -> shiftL# (int2Word# 1#) (wordSize -# 1# -# (word2Int# (clz# w)))
{-# INLINE highestBitMask #-}

-- | \(O(\min(n,W))\). Find the value at a key.
-- Calls 'error' when the element can not be found.
(!) :: IntToIntMap -> Key -> Val
(!) m k = find k m
{-# INLINE (!) #-}

find :: Key -> IntToIntMap -> Val
find (int2Word# -> k) = go
  where
    go (Bin _p m l r) | zero k m  = go l
                      | otherwise = go r
    go (Tip kx x) | isTrue# (k `eqWord#` kx)  = x

    -- Incomplete patterns
    go _ = error ("IntMap.!: key ___ is not an element of the map")

-- * Other stuff taken from IntMap

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
