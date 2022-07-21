{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
module Data.Equality.Graph.Lens where

import qualified Data.Set as S

import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Classes

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes
import Data.Equality.Graph.Memo
import Data.Equality.Graph.Classes
import Data.Equality.Analysis
import {-# SOURCE #-} Data.Equality.Graph (EGraph(..), getClass, setClass)

type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)


_class :: ClassId' k -> Lens' (EGraph l) (EClass l)
_class i  afa s =
    let (i', c) = getClass i s
     in setClass s i' <$> afa c
{-# INLINE _class #-}

_memo :: Lens' (EGraph l) (Memo l)
_memo afa egr = (\m1 -> egr {memo = m1}) <$> afa (memo egr)
{-# INLINE _memo #-}

_data :: Lens' (EClass l) (Domain l)
_data afa EClass{..} = (\d1 -> EClass eClassId eClassNodes d1 eClassParents) <$> afa eClassData
{-# INLINE _data #-}

_parents :: Lens' (EClass l) [(ENode 'Canon l, ClassId' 'Canon)]
_parents afa EClass{..} = EClass eClassId eClassNodes eClassData <$> afa eClassParents
{-# INLINE _parents #-}

_nodes :: (Ord1 l, Functor l) => Lens' (EClass l) (S.Set (ENode k l))
_nodes afa EClass{..} = (\ns -> EClass eClassId ns eClassData eClassParents) <$> afa (S.map loseNodeCanon eClassNodes)
{-# INLINE _nodes #-}


-- | Like @'view'@ but with the arguments flipped
(^.) :: s -> Lens' s a -> a
(^.) s ln = view ln s
infixl 8 ^.
{-# INLINE (^.) #-}

-- | Synonym for @'set'@
(.~) :: Lens' s a -> a -> (s -> s)
(.~) = set
infixr 4 .~
{-# INLINE (.~) #-}

-- | Synonym for @'over'@
(%~) :: Lens' s a -> (a -> a) -> (s -> s)
(%~) = over
infixr 4 %~
{-# INLINE (%~) #-}

view :: Lens' s a -> (s -> a)
view ln = getConst . ln Const
{-# INLINE view #-}

set :: Lens' s a -> a -> (s -> s)
set ln x = over ln (const x)
{-# INLINE set #-}

over :: Lens' s a -> (a -> a) -> (s -> s)
over ln f = runIdentity . ln (Identity . f)
{-# INLINE over #-}

