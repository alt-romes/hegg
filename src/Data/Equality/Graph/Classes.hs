{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
   Equivalence classes
-}
module Data.Equality.Graph.Classes
    ( module Data.Equality.Graph.Classes
    , module Data.Equality.Graph.Classes.Id
    ) where

import qualified Data.Set as S

import Data.Functor.Classes

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes

import Data.Equality.Analysis

-- | E-Class
--
-- @cid@ type of e-class ids
-- @nid@ type of e-node ids
--
-- TODO: REVIEW Canonicality
data EClass l = forall k. EClass
    { eClassId      :: {-# UNPACK #-} !ClassId -- ^ E-class Id
    , eClassNodes   :: !(S.Set (ENode k l))      -- ^ E-nodes in this class
    , eClassData    :: Domain l                -- ^ The analysis data associated with this eclass.
    , eClassParents :: ![(ENode 'Canon l, ClassId' 'Canon)]   -- ^ E-nodes which are parents of (reference) this e-class and their e-class ids. (See EGraph.ENode for why @s ClassId@)
    }

instance (Show (Domain l), Show1 l) => Show (EClass l) where
    show (EClass a b d c) = "Id: " <> show a <> "\nNodes: " <> show b <> "\nParents: " <> show c <> "\nData: " <> show d

