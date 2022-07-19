{-# LANGUAGE AllowAmbiguousTypes #-} -- joinA
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Equality.Analysis where

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes

import {-# SOURCE #-} Data.Equality.Graph (EGraph)

class Analysis l where

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
    -- Note: if modifyA does something to the e-classes that might have them
    -- require updates, modifyA itself should add the parents of the modified
    -- e-class to the worklist.
    --
    -- For example, when pruning an e-class with a constant value of all its
    -- nodes except for the leaf values, we must make sure to add the parents
    -- of the pruned e-class to the worklist.
    --
    -- @
    --  -- Prune all except leaf e-nodes
    --  modify (_class i._nodes %~ S.filter (null . children))
    --  get >>= \egr2 -> addToWorklist (egr2^._class i._parents)
    -- @
    modifyA :: ClassId -> EGraph l -> EGraph l
    modifyA _ = id
