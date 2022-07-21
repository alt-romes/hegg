{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
module Data.Equality.Graph where

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes
import Data.Equality.Graph.Memo
import Data.Equality.Graph.ReprUnionFind
import {-# SOURCE #-} Data.Equality.Graph.ClassList (ClassList)
import {-# SOURCE #-} Data.Equality.Graph.Classes (EClass)

type role EGraph nominal
data EGraph l = EGraph
    { unionFind :: !ReprUnionFind           -- ^ Union find like structure to find canonical representation of an e-class id
    , classes   :: ClassList l              -- ^ Map canonical e-class ids to their e-classes
    , memo      :: Memo l                   -- ^ Hashcons maps all canonical e-nodes to their e-class ids
    , worklist  :: Worklist l               -- ^ e-class ids that needs repair and the class it's in
    , analysisWorklist :: Worklist l        -- ^ like 'worklist' but for analysis repairing
    }

getClass :: ClassId' k -> EGraph s -> (ClassId' 'Canon, EClass s)

setClass :: EGraph s -> ClassId' 'Canon -> EClass s -> EGraph s

-- ROMES:TODO: Should worklist be fully canonical?
type Worklist l = [(ENode 'Canon l, ClassId' 'Canon)]
