{-|
   Equivalence classes
-}
module Data.Equality.Graph.Classes
    ( module Data.Equality.Graph.Classes
    , module Data.Equality.Graph.Classes.Id
    ) where

import qualified Data.Set    as S

import Data.Functor.Classes

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes

-- | E-Class
--
-- @cid@ type of e-class ids
-- @nid@ type of e-node ids
data EClass s = EClass
    { eClassId :: {-# UNPACK #-} !ClassId -- ^ E-class Id
    , eClassNodes :: S.Set (ENode s) -- ^ E-nodes in this class
    , eClassParents :: [(ENode s, ClassId)] -- ^ E-nodes which are parents of (reference) this e-class and their e-class ids. (See EGraph.ENode for why @s ClassId@)
    }

instance Show1 l => Show (EClass l) where
    show (EClass a b c) = "Id: " <> show a <> "\nNodes: " <> show b <> "\nParents: " <> show c

