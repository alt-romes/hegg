{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Data.Equality.Utils.IntToIntMap where

import GHC.Exts
import Data.Bits
import Data.Array.Unboxed
import Data.Array.IArray as IA
import Data.Array.MArray as M
import Data.Array.Base
import Data.Array.ST

-- | A map of integers to integers
type IntToIntMap :: TYPE ('BoxedRep 'Unlifted)
data IntToIntMap = Bin Prefix Mask IntToIntMap IntToIntMap
                 | Tip InternalKey (UArray Int Int)
                 | Nil

type Prefix      = Word#
type Mask        = Word#
type InternalKey = Word#
type Key         = Int#
type Val         = Int#

-- Could do this with IntMap.Internal?

-- The intmap maps to an array of ints of size [1024]. The first 10 bits are
-- used to index the array, and the remaining ones are shifted 10 bits to the
-- right and are used to get the array out of the intmap

-- delete :: Key -> IntToIntMap -> IntToIntMap
-- delete k = delete' (int2Word# k)
-- {-# INLINE delete #-}

-- delete' :: InternalKey -> IntToIntMap -> IntToIntMap
-- delete' k t@(Bin p m l r)
--   | nomatch k p m = t
--   | zero k m      = binCheckLeft p m (delete' k l) r
--   | otherwise     = binCheckRight p m l (delete' k r)
-- delete' k t@(Tip ky _)
--   | isTrue# (k `eqWord#` ky) = Nil
--   | otherwise      = t
-- delete' _k Nil = Nil

-- binCheckLeft only checks that the left subtree is non-empty
binCheckLeft :: Prefix -> Mask -> IntToIntMap -> IntToIntMap -> IntToIntMap
binCheckLeft _ _ Nil r = r
binCheckLeft p m l r   = Bin p m l r
{-# INLINE binCheckLeft #-}

-- binCheckRight only checks that the right subtree is non-empty
binCheckRight :: Prefix -> Mask -> IntToIntMap -> IntToIntMap -> IntToIntMap
binCheckRight _ _ l Nil = l
binCheckRight p m l r   = Bin p m l r
{-# INLINE binCheckRight #-}

insert :: Key -> Val -> IntToIntMap -> IntToIntMap
insert x@(int2Word# -> k) = insert' (k `shiftL#` 10#) (I# (x `andI#` 1023#))
{-# INLINE insert #-}

-- | The first key is the prefix for the intmap, the second key is the index in the array
insert' :: Word# -> Int -> Val -> IntToIntMap -> IntToIntMap
insert' k0 k1 x t@(Bin p m l r)
  | nomatch k0 p m = link k0 (Tip k0 (newLeafArrayWithXAt k1 x)) p t
  | zero k0 m      = Bin p m (insert' k0 k1 x l) r
  | otherwise      = Bin p m l (insert' k0 k1 x r)
insert' k0 k1 x t@(Tip ky arr)
  | isTrue# (k0 `eqWord#` ky) = Tip k0 (arr // [(k1,I# x)])
  | otherwise                 = link k0 (Tip k0 (newLeafArrayWithXAt k1 x)) ky t
insert' k0 k1 x Nil = Tip k0 $ newLeafArrayWithXAt k1 x

newLeafArrayWithXAt :: Int -> Val -> UArray Int Int
newLeafArrayWithXAt k1 x = runSTUArray $ do
  a <- newArray_ (0,1023)
  unsafeWrite a k1 (I# x)
  return a
{-# INLINE newLeafArrayWithXAt #-}


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
find k = find' (int2Word# k `shiftL#` 10#) (I# (k `andI#` 1023#))
{-# INLINE find #-}

find' :: Word# -> Int -> IntToIntMap -> Val
find' k0 k1 (Bin _p m l r)
  | zero k0 m  = find' k0 k1 l
  | otherwise  = find' k0 k1 r
find' k0 k1 (Tip kx arr) | isTrue# (k0 `eqWord#` kx) = case arr IA.! k1 of I# v -> v
find' _ _ _ = error ("IntMap.!: key ___ is not an element of the map")

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

-- * Utils

unliftedFoldr :: forall a {b :: TYPE ('BoxedRep 'Unlifted)} . (a -> b -> b) -> b -> [a] -> b 
unliftedFoldr k z = go
  where
    go []     = z
    go (y:ys) = y `k` go ys
