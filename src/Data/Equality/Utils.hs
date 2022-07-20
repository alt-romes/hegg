{-# LANGUAGE StandaloneDeriving #-}
module Data.Equality.Utils where

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
