{-# LANGUAGE Rank2Types #-}
module Data.Equality.Graph.Lens where

import Data.Functor.Identity
import Data.Functor.Const

import Data.Equality.Graph

type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)


_class :: ClassId -> Lens' (EGraph l) (EClass l)
_class i afa s =
    let (i', c) = getClass i s
     in setClass s i' <$> afa c

-- | Like @'view'@ but with the arguments flipped
(^.) :: s -> Lens' s a -> a
(^.) s ln = view ln s

-- | Synonym for @'set'@
(.~) :: Lens' s a -> a -> (s -> s)
(.~) = set

-- | Synonym for @'over'@
(%~) :: Lens' s a -> (a -> a) -> (s -> s)
(%~) = over

view :: Lens' s a -> (s -> a)
view ln = getConst . ln Const
{-# INLINE view #-}

set :: Lens' s a -> a -> (s -> s)
set ln x = over ln (const x)
{-# INLINE set #-}

over :: Lens' s a -> (a -> a) -> (s -> s)
over ln f = runIdentity . ln (Identity . f)
{-# INLINE over #-}

