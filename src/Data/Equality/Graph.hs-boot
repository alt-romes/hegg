{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
module Data.Equality.Graph where

import qualified Data.Map as M
import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes
import Data.Equality.Graph.ReprUnionFind
import {-# SOURCE #-} Data.Equality.Graph.Classes (EClass)

type role EGraph nominal
data EGraph l = EGraph
    { unionFind :: !ReprUnionFind              -- ^ Union find like structure to find canonical representation of an e-class id
    , classes   :: !(ClassIdMap (EClass l))    -- ^ Map canonical e-class ids to their e-classes
    , memo      :: !(Memo l)                   -- ^ Hashcons maps all canonical e-nodes to their e-class ids
    , worklist  :: [(ENode l, ClassId)]        -- ^ e-class ids that needs repair and the class it's in
    , analysisWorklist :: [(ENode l, ClassId)] -- ^ like 'worklist' but for analysis repairing
    }

getClass :: ClassId -> EGraph s -> (ClassId, EClass s)

setClass :: EGraph s -> ClassId -> EClass s -> EGraph s

type Memo l = M.Map (ENode l) ClassId
