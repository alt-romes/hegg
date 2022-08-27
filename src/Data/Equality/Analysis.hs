{-# LANGUAGE AllowAmbiguousTypes #-} -- joinA
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|

E-class analysis, which allows the concise expression of a program analysis over
the e-graph.

An e-class analysis resembles abstract interpretation lifted to the e-graph
level, attaching analysis data from a semilattice to each e-class.

The e-graph maintains and propagates this data as e-classes get merged and new
e-nodes are added.

Analysis data can be used directly to modify the e-graph, to inform how or if
rewrites apply their right-hand sides, or to determine the cost of terms during
the extraction process.

References: https://arxiv.org/pdf/2004.03082.pdf

-}
module Data.Equality.Analysis where

import Data.Kind (Type)

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes

import {-# SOURCE #-} Data.Equality.Graph.Internal (EGraph)

-- | The e-class analysis defined for a language @l@.
class Eq (Domain l) => Analysis (l :: Type -> Type) where

    -- | Domain of data stored in e-class according to e-class analysis
    type Domain l

    -- | When a new e-node is added into a new, singleton e-class, construct a
    -- new value of the domain to be associated with the new e-class, typically
    -- by accessing the associated data of n's children
    makeA :: ENode l -> EGraph l -> Domain l

    -- | When e-classes c1 c2 are being merged into c, join d_c1 and
    -- d_c2 into a new value d_c to be associated with the new
    -- e-class c
    joinA :: Domain l -> Domain l -> Domain l

    -- | Optionaly modify the e-class c (based on d_c), typically by adding an
    -- e-node to c. Modify should be idempotent if no other changes occur to
    -- the e-class, i.e., modify(modify(c)) = modify(c)
    --
    -- === Example
    --
    -- Pruning an e-class with a constant value of all its nodes except for the
    -- leaf values
    --
    -- @
    --  -- Prune all except leaf e-nodes
    --  modify (_class i._nodes %~ S.filter (null . children))
    -- @
    modifyA :: ClassId -> EGraph l -> EGraph l
    modifyA _ = id
    {-# INLINE modifyA #-}
