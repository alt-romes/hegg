module EGraph.ENode where

import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

import EGraph.EClass

-- | E-node
--
-- @s@ type of e-node term
-- @nid@ type of e-node ids
data ENode s nid = ENode
    { eNodeId :: !nid
    , eNodeTerm :: s
    , eNodeChildren :: [ClassId]
    }

instance (Show s, Show nid) => Show (ENode s nid) where
    show (ENode i t c) = "Id: " <> show i <> "\nTerm: " <> show t <> "\nChildren: " <> show c

instance Eq s => Eq (ENode s nid) where
    (ENode _ s xs) == (ENode _ s' xs') = s == s' && xs == xs'

instance (Eq s, Ord nid) => Ord (ENode s nid) where
    (ENode nid _ _) <= (ENode nid' _ _) = nid <= nid'

