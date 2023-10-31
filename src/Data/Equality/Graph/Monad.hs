{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-|
   Monadic interface to e-graph stateful computations
 -}
module Data.Equality.Graph.Monad
  (
    -- * Threading e-graphs in a stateful computation
    --
    -- | These are the same operations over e-graphs as in 'Data.Equality.Graph',
    -- but defined in the context of a 'State' monad threading around the e-graph.
    egraph
  , represent
  , add
  , merge
  , rebuild
  , EG.canonicalize
  , EG.find
  , EG.emptyEGraph

    -- * E-graph transformations for monadic analysis
    --
    -- | The same e-graph operations in a stateful computation threading around
    -- the e-graph, but for 'Analysis' defined monadically ('AnalysisM').
  , representM, addM, mergeM, rebuildM

  -- * E-graph stateful computations
  , EGraphM
  , EGraphMT
  , runEGraphM
  , runEGraphMT

  -- * E-graph definition re-export
  , EG.EGraph

  -- * 'State' monad re-exports
  , modify, get, gets
  ) where

import Control.Monad ((>=>))
import Control.Monad.Trans.State.Strict

import Data.Equality.Utils (Fix, cata)

import Data.Equality.Analysis
import qualified Data.Equality.Analysis.Monadic as AM
import Data.Equality.Graph (EGraph, ClassId, Language, ENode(..))
import qualified Data.Equality.Graph as EG

-- | E-graph stateful computation
type EGraphM a l = State (EGraph a l)
-- | E-graph stateful computation over an arbitrary monad
type EGraphMT a l = StateT (EGraph a l)

-- | Run EGraph computation on an empty e-graph
--
-- === Example
-- @
-- egraph $ do
--  id1 <- represent t1
--  id2 <- represent t2
--  merge id1 id2
-- @
egraph :: Language l => EGraphM anl l a -> (a, EGraph anl l)
egraph = runEGraphM EG.emptyEGraph
{-# INLINE egraph #-}

-- | Represent an expression (@Fix l@) in an e-graph by recursively
-- representing sub expressions
represent :: (Analysis anl l, Language l) => Fix l -> EGraphM anl l ClassId
represent = cata $ sequence >=> add . Node
{-# INLINE represent #-}

-- | Add an e-node to the e-graph
add :: (Analysis anl l, Language l) => ENode l -> EGraphM anl l ClassId
add = StateT . fmap pure . EG.add
{-# INLINE add #-}

-- | Merge two e-classes by id
--
-- E-graph invariants may be broken by merging, and 'rebuild' should be used
-- /eventually/ to restore them
merge :: (Analysis anl l, Language l) => ClassId -> ClassId -> EGraphM anl l ClassId
merge a b = StateT (pure <$> EG.merge a b)
{-# INLINE merge #-}

-- | Rebuild: Restore e-graph invariants
--
-- E-graph invariants are traditionally maintained after every merge, but we
-- allow operations to temporarilly break the invariants (specifically, until we call
-- 'rebuild')
--
-- The paper describing rebuilding in detail is https://arxiv.org/abs/2004.03082
rebuild :: (Analysis anl l, Language l) => EGraphM anl l ()
rebuild = StateT (pure . ((),). EG.rebuild)
{-# INLINE rebuild #-}

-- | Run 'EGraphM' computation on a given e-graph
runEGraphM :: EGraph anl l -> EGraphM anl l a -> (a, EGraph anl l)
runEGraphM = flip runState
{-# INLINE runEGraphM #-}

--------------------------------------------------------------------------------
-- Monadic Analysis interface

-- | Run 'EGraphM' computation on a given e-graph over a monadic analysis
runEGraphMT :: EGraph anl l -> EGraphMT anl l m a -> m (a, EGraph anl l)
runEGraphMT = flip runStateT
{-# INLINE runEGraphMT #-}

-- | Like 'represent', but for a monadic analysis
representM :: (AM.AnalysisM m anl l, Language l) => Fix l -> EGraphMT anl l m ClassId
representM = StateT . EG.representM
{-# INLINE representM #-}

-- | Like 'add', but for a monadic analysis
addM :: (AM.AnalysisM m anl l, Language l) => ENode l -> EGraphMT anl l m ClassId
addM = StateT . EG.addM
{-# INLINE addM #-}

-- | Like 'merge', but for a monadic analysis
mergeM :: (AM.AnalysisM m anl l, Language l) => ClassId -> ClassId -> EGraphMT anl l m ClassId
mergeM a b = StateT (EG.mergeM a b)
{-# INLINE mergeM #-}

-- | Like 'rebuild', but for a monadic analysis
rebuildM :: (AM.AnalysisM m anl l, Language l) => EGraphMT anl l m ()
rebuildM = StateT (fmap ((),) . EG.rebuildM)
{-# INLINE rebuildM #-}
