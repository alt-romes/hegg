{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-|
   Equivalence classes
-}
module Data.Equality.Graph.Classes where

import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

type ClassId = Int
type ClassIdMap = IM.IntMap

-- | E-Class
--
-- @cid@ type of e-class ids
-- @nid@ type of e-node ids
data EClass s = EClass
    { eClassId :: {-# UNPACK #-} !ClassId -- ^ E-class Id
    , eClassNodes :: S.Set (s ClassId) -- ^ E-nodes in this class
    , eClassParents :: [(s ClassId, ClassId)] -- ^ E-nodes which are parents of (reference) this e-class and their e-class ids. (See EGraph.ENode for why @s ClassId@)
    }

instance Show (s ClassId) => Show (EClass s) where
    show (EClass a b c) = "Id: " <> show a <> "\nNodes: " <> show b <> "\nParents: " <> show c

