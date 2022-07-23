{-# LANGUAGE StandaloneDeriving #-}
module Data.Equality.Utils where

import Data.Foldable
import Data.Bits

import qualified Data.Set    as S
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

hash :: String -> Int
hash = foldl' (\h c -> 33*h `xor` fromEnum c) 5381
{-# INLINE hash #-}
