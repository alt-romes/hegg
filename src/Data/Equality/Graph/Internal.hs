{-# LANGUAGE UndecidableInstances #-} -- tmp show
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK hide #-}
{-|
 Non-abstract definition of e-graphs
 -}
module Data.Equality.Graph.Internal where

import Data.Functor.Classes

import Data.Equality.Graph.ReprUnionFind
import Data.Equality.Graph.Classes
import Data.Equality.Graph.Nodes
import Data.Equality.Analysis

-- | E-graph representing terms of language @l@.
--
-- Intuitively, an e-graph is a set of equivalence classes (e-classes). Each e-class is a
-- set of e-nodes representing equivalent terms from a given language, and an e-node is a function
-- symbol paired with a list of children e-classes.
data EGraph l = EGraph
    { unionFind :: !ReprUnionFind           -- ^ Union find like structure to find canonical representation of an e-class id
    , classes   :: !(ClassIdMap (EClass l)) -- ^ Map canonical e-class ids to their e-classes
    , memo      :: !(Memo l)                -- ^ Hashcons maps all canonical e-nodes to their e-class ids
    , worklist  :: !(Worklist l)            -- ^ Worklist of e-class ids that need to be upward merged
    , analysisWorklist :: !(Worklist l)     -- ^ Like 'worklist' but for analysis repairing
    }

-- | The hashcons 𝐻  is a map from e-nodes to e-class ids
type Memo l = NodeMap l ClassId

-- | Maintained worklist of e-class ids that need to be “upward merged”
type Worklist l = NodeMap l ClassId

instance (Show (Domain l), Show1 l) => Show (EGraph l) where
    show (EGraph a b c d e) =
        "UnionFind: " <> show a <>
            "\n\nE-Classes: " <> show b <>
                "\n\nHashcons: " <> show c <>
                    "\n\nWorklist: " <> show d <>
                        "\n\nAnalWorklist: " <> show e
