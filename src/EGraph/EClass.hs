{-# LANGUAGE LambdaCase #-}
module EGraph.EClass where

import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

type ClassId = Int
type ClassIdMap = IM.IntMap

-- | E-Class
--
-- @cid@ type of e-class ids
-- @nid@ type of e-node ids
data EClass nid = EClass
    { eClassId :: {-# UNPACK #-} !ClassId -- ^ E-class Id
    , eClassNodes :: S.Set nid -- ^ E-nodes in this class
    , eClassParents :: [(nid, ClassId)] -- ^ E-nodes which are parents of (reference) this e-class and their e-class ids
    }

instance Show nid => Show (EClass nid) where
    show (EClass a b c) = "Id: " <> show a <> "\nNodes: " <> show b <> "\nParents: " <> show c

