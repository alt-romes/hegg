{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Hashcons mapping canonical e-nodes to their e-class ids

This module provides a typed wrapper on the hashcons that guarantees one can
only insert canonical e-nodes there
-}
module Data.Equality.Graph.Memo
    ( Memo(unMemo)
    , lookup
    , insert
    , delete
    , insertLookup
    , size
    ) where

import Prelude hiding (lookup)

import Data.Bifunctor
import Data.Functor.Classes

import qualified Data.Map as M

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes

-- | A hashcons (memo) maps canonical e-nodes to their e-class ids.
--
-- Operations on 'Memo' require that the e-node proves that is canonical
newtype Memo l = Memo { unMemo :: M.Map (ENode 'Canon l) (ClassId' 'Canon) }
    deriving (Semigroup, Monoid, Show)

lookup :: Ord1 l => ENode 'Canon l -> Memo l -> Maybe (ClassId' 'Canon)
lookup n (Memo m) = M.lookup n m
{-# INLINE lookup #-}

insert :: Ord1 l => ENode 'Canon l -> ClassId' 'Canon -> Memo l -> Memo l
insert en i (Memo m) = Memo (M.insert en i m)
{-# INLINE insert #-}

delete :: Ord1 l => ENode 'Canon l -> Memo l -> Memo l
delete en (Memo m) = Memo (M.delete en m)
{-# INLINE delete #-}

size :: Memo l -> Int
size = M.size . unMemo
{-# INLINE size #-}

insertLookup :: Ord1 l => ENode 'Canon l -> ClassId' 'Canon -> Memo l -> (Maybe (ClassId' 'Canon), Memo l)
insertLookup en cid (Memo m) = second Memo $ M.insertLookupWithKey (\_ a _ -> a) en cid m
{-# INLINE insertLookup #-}
