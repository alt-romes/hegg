{-# LANGUAGE TupleSections #-}
{-|
   Monadic interface to e-graph stateful computations
 -}
module Data.Equality.Graph.Monad
  ( egraph
  , represent
  , add
  , merge
  , rebuild
  , EG.canonicalize
  , EG.find
  , EG.emptyEGraph

  -- * E-graph stateful computations
  , EGraphM
  , runEGraphM

  -- * E-graph definition re-export
  , EG.EGraph

  -- * 'State' monad re-exports
  , modify, get, gets
  ) where

import Control.Monad ((>=>))
import Control.Monad.Trans.State.Strict

import Data.Equality.Utils (Fix, cata)

import Data.Equality.Graph (EGraph, ClassId, Language, ENode(..))
import qualified Data.Equality.Graph as EG

-- | E-graph stateful computation
type EGraphM s = State (EGraph s)

-- | Run EGraph computation on an empty e-graph
--
-- === Example
-- @
-- egraph $ do
--  id1 <- represent t1
--  id2 <- represent t2
--  merge id1 id2
-- @
egraph :: Language l => EGraphM l a -> (a, EGraph l)
egraph = runEGraphM EG.emptyEGraph
{-# INLINE egraph #-}

-- | Represent an expression (@Fix l@) in an e-graph by recursively
-- representing sub expressions
represent :: Language l => Fix l -> EGraphM l ClassId
represent = cata $ sequence >=> add . Node
{-# INLINE represent #-}

-- | Add an e-node to the e-graph
add :: Language l => ENode l -> EGraphM l ClassId
add = StateT . fmap pure . EG.add
{-# INLINE add #-}

-- | Merge two e-classes by id
--
-- E-graph invariants may be broken by merging, and 'rebuild' should be used
-- /eventually/ to restore them
merge :: Language l => ClassId -> ClassId -> EGraphM l ClassId
merge a b = StateT (pure <$> EG.merge a b)
{-# INLINE merge #-}

-- | Rebuild: Restore e-graph invariants
--
-- E-graph invariants are traditionally maintained after every merge, but we
-- allow operations to temporarilly break the invariants (specifically, until we call
-- 'rebuild')
--
-- The paper describing rebuilding in detail is https://arxiv.org/abs/2004.03082
rebuild :: Language l => EGraphM l ()
rebuild = StateT (pure . ((),). EG.rebuild)
{-# INLINE rebuild #-}

-- | Run 'EGraphM' computation on a given e-graph
runEGraphM :: EGraph l -> EGraphM l a -> (a, EGraph l)
runEGraphM = flip runState
{-# INLINE runEGraphM #-}
