{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
module Data.Equality.Graph where

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes
import Data.Equality.Graph.ReprUnionFind
import {-# SOURCE #-} Data.Equality.Graph.Classes (EClass)

type role EGraph nominal
data EGraph l = EGraph
    { unionFind :: !ReprUnionFind
    , classes   :: !(ClassIdMap (EClass l))
    , memo      :: !(Memo l)
    , worklist  :: !(Worklist l)
    , analysisWorklist :: !(Worklist l)
    }

find :: ClassId -> EGraph l -> ClassId

type Memo l = NodeMap l ClassId
type Worklist l = NodeMap l ClassId
