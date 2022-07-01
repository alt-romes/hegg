module EGraph.ENode where

import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

import EGraph.EClass

-- | E-node
--
-- @s@ type of e-node term
-- @nid@ type of e-node ids
--
-- Note: 2 equal e-nodes must have the same e-node id
data ENode s = ENode
    { eNodeId :: s
    , eNodeChildren :: [ClassId]
    }

instance Show s => Show (ENode s) where
    show (ENode t c) = "\nTerm: " <> show t <> "\nChildren: " <> show c

instance Eq s => Eq (ENode s) where
    (ENode s _) == (ENode s' _) = s == s'

instance Ord s => Ord (ENode s) where
    (ENode s _) <= (ENode s' _) = s <= s'

