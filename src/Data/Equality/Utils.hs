{-# LANGUAGE StandaloneDeriving #-}
module Data.Equality.Utils where

import GHC.Conc

import Data.Foldable
import Data.Bits

import qualified Data.Set    as S
import qualified Data.IntSet as IS
import qualified Data.Map    as M
import qualified Data.Map.Strict as MS
import Data.Functor.Classes

newtype Fix f = Fix { unFix :: f (Fix f) }

instance Eq1 f => Eq (Fix f) where
    (==) (Fix a) (Fix b) = liftEq (==) a b
    {-# INLINE (==) #-}

instance Show1 f => Show (Fix f) where
    showsPrec d (Fix f) = liftShowsPrec showsPrec showList d f
    {-# INLINE showsPrec #-}

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix
{-# INLINE cata #-}

ordNub :: Ord a => [a] -> [a]
ordNub = S.toList . S.fromList
{-# INLINE ordNub #-}

hashString :: String -> Int
hashString = foldl' (\h c -> 33*h `xor` fromEnum c) 5381
{-# INLINE hashString #-}

-- We don't have the parallel package, so roll our own simple parMap
parMap :: (a -> b) -> [a] -> [b]
parMap _ [] = []
parMap f (x:xs) = fx `par` (fxs `pseq` (fx : fxs))
    where fx = f x; fxs = parMap f xs

-- | Insert and lookup in a Map
insertLookup :: Ord k => k -> a -> M.Map k a -> (Maybe a, M.Map k a)
insertLookup = M.insertLookupWithKey (\_ a _ -> a)
{-# INLINE insertLookup #-}

-- | Strict insert and lookup in a Map
insertLookup' :: Ord k => k -> a -> M.Map k a -> (Maybe a, M.Map k a)
insertLookup' = MS.insertLookupWithKey (\_ a _ -> a)
{-# INLINE insertLookup' #-}

toSet :: (Ord a, Foldable f) => f a -> S.Set a
toSet = foldl' (flip S.insert) mempty
{-# INLINE toSet #-}

toIntSet :: (Foldable f) => f Int -> IS.IntSet
toIntSet = foldl' (flip IS.insert) mempty
{-# INLINE toIntSet #-}
