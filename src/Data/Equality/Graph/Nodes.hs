{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-|

Module defining e-nodes ('ENode'), the e-node function symbol ('Operator'), and
mappings from e-nodes ('NodeMap').

-}
module Data.Equality.Graph.Nodes where

import Data.Foldable
import Data.Bifunctor

import Data.Kind

import Control.Monad (void)

import qualified Data.Map.Strict as M

import Data.Equality.Graph.Classes.Id


-- * E-node

-- | An e-node is a function symbol paired with a list of children e-classes.
-- 
-- We define an e-node to be the base functor of some recursive data type
-- parametrized over 'ClassId', i.e. all recursive fields are rather e-class ids.
newtype ENode l = Node { unNode :: l ClassId }

deriving instance Eq (l ClassId) => (Eq (ENode l))
deriving instance Ord (l ClassId) => (Ord (ENode l))
deriving instance Show (l ClassId) => (Show (ENode l))

-- | Get the children e-class ids of an e-node
children :: Traversable l => ENode l -> [ClassId]
children = toList . unNode
{-# INLINE children #-}

-- * Operator

-- | An operator is solely the function symbol part of the e-node. Basically,
-- this means children e-classes are ignored.
newtype Operator l = Operator { unOperator :: l () }

deriving instance Eq (l ()) => (Eq (Operator l))
deriving instance Ord (l ()) => (Ord (Operator l))
deriving instance Show (l ()) => (Show (Operator l))

-- | Get the operator (function symbol) of an e-node
operator :: Traversable l => ENode l -> Operator l
operator = Operator . void . unNode
{-# INLINE operator #-}

-- * Node Map

-- | A mapping from e-nodes of @l@ to @a@
newtype NodeMap (l :: Type -> Type) a = NodeMap { unNodeMap :: M.Map (ENode l) a }
-- TODO: Investigate whether it would be worth it requiring a trie-map for the
-- e-node definition. Probably it isn't better since e-nodes aren't recursive.
  deriving (Functor, Foldable, Traversable)

deriving instance (Show a, Show (l ClassId)) => Show (NodeMap l a)
deriving instance Ord (l ClassId) => Semigroup (NodeMap l a)
deriving instance Ord (l ClassId) => Monoid (NodeMap l a)

-- | Insert a value given an e-node in a 'NodeMap'
insertNM :: Ord (l ClassId) => ENode l -> a -> NodeMap l a -> NodeMap l a
insertNM e v (NodeMap m) = NodeMap (M.insert e v m)
{-# INLINE insertNM #-}

-- | Lookup an e-node in a 'NodeMap'
lookupNM :: Ord (l ClassId) => ENode l -> NodeMap l a -> Maybe a
lookupNM e = M.lookup e . unNodeMap
{-# INLINE lookupNM #-}

-- | Delete an e-node in a 'NodeMap'
deleteNM :: Ord (l ClassId) => ENode l -> NodeMap l a -> NodeMap l a
deleteNM e (NodeMap m) = NodeMap (M.delete e m)
{-# INLINE deleteNM #-}

-- | Insert a value and lookup by e-node in a 'NodeMap'
insertLookupNM :: Ord (l ClassId) => ENode l -> a -> NodeMap l a -> (Maybe a, NodeMap l a)
insertLookupNM e v (NodeMap m) = second NodeMap $ M.insertLookupWithKey (\_ a _ -> a) e v m
{-# INLINE insertLookupNM #-}

-- | As 'Data.Map.foldlWithKeyNM'' but in a 'NodeMap'
foldlWithKeyNM' :: Ord (l ClassId) => (b -> ENode l -> a -> b) -> b -> NodeMap l a -> b 
foldlWithKeyNM' f b = M.foldlWithKey' f b . unNodeMap
{-# INLINE foldlWithKeyNM' #-}

-- | As 'Data.Map.foldrWithKeyNM'' but in a 'NodeMap'
foldrWithKeyNM' :: Ord (l ClassId) => (ENode l -> a -> b -> b) -> b -> NodeMap l a -> b 
foldrWithKeyNM' f b = M.foldrWithKey' f b . unNodeMap
{-# INLINE foldrWithKeyNM' #-}

-- | Get the number of entries in a 'NodeMap'.
--
-- This operation takes constant time (__O(1)__)
sizeNM :: NodeMap l a -> Int
sizeNM = M.size . unNodeMap
{-# INLINE sizeNM #-}

-- | As 'Data.Map.traverseWithKeyNM' but in a 'NodeMap'
traverseWithKeyNM :: Applicative t => (ENode l -> a -> t b) -> NodeMap l a -> t (NodeMap l b) 
traverseWithKeyNM f (NodeMap m) = NodeMap <$> M.traverseWithKey f m
{-# INLINE traverseWithKeyNM #-}

-- Node Set

-- newtype NodeSet l a = NodeSet { unNodeSet :: IM.IntMap (a, ENode l) }
--   deriving (Semigroup, Monoid)

-- insertNS :: Hashable1 l => ENode l -> NodeSet l -> NodeSet l
-- insertNS v = NodeSet . IM.insert (hashNode v) v . unNodeSet
