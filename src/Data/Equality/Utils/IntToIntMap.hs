{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Data.Equality.Utils.IntToIntMap where

import GHC.Exts
import Data.Bits
import Unsafe.Coerce

-- | A map of integers to integers
type IntToIntMap :: TYPE ('BoxedRep 'Unlifted)
data IntToIntMap = Bin Prefix Mask IntToIntMap IntToIntMap
                 | Tip InternalKey ByteArray#
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

-- shiftAmount = 9#
-- andAmount   = 511#

find :: Key -> IntToIntMap -> Val
find k = find' (int2Word# k `shiftL#` 8#) (k `andI#` 255#)
{-# INLINE find #-}

insert :: Key -> Val -> IntToIntMap -> IntToIntMap
insert x@(int2Word# -> k) = insert' (k `shiftL#` 8#) (x `andI#` 255#)
{-# INLINE insert #-}

-- Warning: The destructive update might screw up parallelism? But are there destructive updates??

newLeafArrayWithXAt :: Int# -> Val -> ByteArray#
newLeafArrayWithXAt k1 x = case intSize' of
  I# intSize -> case runRW# $ \s0 -> do
      let !(# s1, a #)  = newPinnedByteArray# (intSize *# 256#) s0
          !s2           = writeIntArray# a k1 x s1
          !(# s3, fa #) = unsafeFreezeByteArray# a s2
       in (# s3 , fa #)
    of
    (# _, a #) -> a
{-# INLINE newLeafArrayWithXAt #-}

intSize' :: Int
intSize' = finiteBitSize (0 :: Int)
{-# INLINE intSize' #-}

-- Gotta make sure the old array is never used again.... -o the argument should be used linearly... TODO
linearUpdateArray :: ByteArray# %1 -> Int# -> Val -> ByteArray#
linearUpdateArray = toLinear (\a k1 x ->
  case runRW# $ \s0 -> do
       (# writeIntArray# (unsafeCoerce# a) k1 x s0, unsafeCoerce# a #)
  of
  (# _, a' #) -> a')
{-# INLINE linearUpdateArray #-}

-- | Converts an unrestricted function into a linear function
toLinear ::
  forall
    (r1 :: RuntimeRep)
    (r2 :: RuntimeRep)
    (a :: TYPE r1)
    (b :: TYPE r2)
    p
    x.
  (a %p -> b) %1 ->
  (a %x -> b)
toLinear f = case unsafeEqualityProof @p @x of
  UnsafeRefl -> f

-- copyUpdateArray :: ByteArray# -> Int# -> Val -> ByteArray#
-- copyUpdateArray arr0 k1 x = case intSize' of
--   I# intSize -> case runRW# $ \s0 -> do
--       let !(# s1, a #)  = newByteArray# (intSize *# 64#) s0
--           !s2           = copyByteArray# arr0 0# a 0# (sizeofByteArray# arr0) s1
--           !s3           = writeIntArray# a k1 x s2
--           !(# s4, fa #) = unsafeFreezeByteArray# a s3
--        in (# s4 , fa #)
--     of
--     (# _, a #) -> a
-- {-# INLINE copyUpdateArray #-}

-- | The first key is the prefix for the intmap, the second key is the index in the array
insert' :: Word# -> Int# -> Val -> IntToIntMap -> IntToIntMap
insert' k0 k1 x t@(Bin p m l r)
  | nomatch k0 p m = link k0 (Tip k0 (newLeafArrayWithXAt k1 x)) p t
  | zero k0 m      = Bin p m (insert' k0 k1 x l) r
  | otherwise      = Bin p m l (insert' k0 k1 x r)
insert' k0 k1 x t@(Tip ky arr)
  | isTrue# (k0 `eqWord#` ky) = Tip k0 (linearUpdateArray arr k1 x)
  | otherwise                 = link k0 (Tip k0 (newLeafArrayWithXAt k1 x)) ky t
insert' k0 k1 x Nil = Tip k0 (newLeafArrayWithXAt k1 x)

-- | \(O(\min(n,W))\). Find the value at a key.
-- Calls 'error' when the element can not be found.
(!) :: IntToIntMap -> Key -> Val
(!) m k = find k m
{-# INLINE (!) #-}

find' :: Word# -> Int# -> IntToIntMap -> Val
find' k0 k1 (Bin _p m l r)
  | zero k0 m  = find' k0 k1 l
  | otherwise  = find' k0 k1 r
find' k0 k1 (Tip kx arr) | isTrue# (k0 `eqWord#` kx) = indexIntArray# arr k1
find' _ _ _ = error ("IntMap.!: key ___ is not an element of the map")

-- * Other stuff taken from IntMap


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

-- binCheckRight only checks that the right subtree is non-empty
binCheckRight :: Prefix -> Mask -> IntToIntMap -> IntToIntMap -> IntToIntMap
binCheckRight _ _ l Nil = l
binCheckRight p m l r   = Bin p m l r
{-# INLINE binCheckRight #-}

-- binCheckLeft only checks that the left subtree is non-empty
binCheckLeft :: Prefix -> Mask -> IntToIntMap -> IntToIntMap -> IntToIntMap
binCheckLeft _ _ Nil r = r
binCheckLeft p m l r   = Bin p m l r
{-# INLINE binCheckLeft #-}

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
