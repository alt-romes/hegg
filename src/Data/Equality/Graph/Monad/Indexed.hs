{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Data.Equality.Graph.Monad.Indexed
Description : Monadic interface for type-indexed e-graphs
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides a monadic interface to type-indexed e-graph operations,
similar to 'Data.Equality.Graph.Monad' but for type-indexed languages.

Example usage:

@
result = egraphI $ do
    id1 <- addIM (TConst 1)
    id2 <- addIM (TConst 2)
    id3 <- addIM (TAdd id1 id2)
    mergeIM id1 id2
    rebuildIM
    return id3
@
-}
module Data.Equality.Graph.Monad.Indexed
    ( -- * E-graph stateful computation
      EGraphIM
    , egraphI
    , runEGraphIM
      -- * Operations
    , addIM
    , mergeIM
    , rebuildIM
    , findIM
    , canonicalizeIM
      -- * State operations
    , getEGraphIM
    , modifyEGraphIM
      -- * Re-exports
    , module Control.Monad.Trans.State.Strict
    ) where

import Control.Monad.Trans.State.Strict
import Data.Kind (Type)

import Data.Equality.Graph (ClassId)
import Data.Equality.Graph.Indexed
import Data.Equality.Language.Indexed
import Data.Equality.Analysis.Indexed
import Data.Equality.Utils.Singleton

-- | Type-indexed e-graph stateful computation.
type EGraphIM a l = State (EGraphI a l)

-- | Run an indexed e-graph computation on an empty e-graph.
--
-- === Example
-- @
-- egraphI $ do
--   id1 <- addIM (TConst 1)
--   id2 <- addIM (TConst 2)
--   mergeIM id1 id2
-- @
egraphI :: (LanguageI l, SOrd k)
        => EGraphIM a (l :: k -> Type -> Type) r
        -> (r, EGraphI a l)
egraphI = runEGraphIM emptyEGraphI
{-# INLINE egraphI #-}

-- | Run an indexed e-graph computation on a given e-graph.
runEGraphIM :: EGraphI a l -> EGraphIM a l r -> (r, EGraphI a l)
runEGraphIM = flip runState
{-# INLINE runEGraphIM #-}

-- | Add a type-indexed e-node in the state monad.
addIM :: forall k a (l :: k -> Type -> Type) dom.
         (LanguageI l, SOrd k, AnalysisI a l, SingI dom)
      => l dom ClassId
      -> EGraphIM a l ClassId
addIM node = state (addI node)
{-# INLINE addIM #-}

-- | Merge two e-classes in the state monad.
mergeIM :: forall k a (l :: k -> Type -> Type).
           (LanguageI l, SOrd k, AnalysisI a l)
        => ClassId
        -> ClassId
        -> EGraphIM a l ClassId
mergeIM c1 c2 = state (mergeI c1 c2)
{-# INLINE mergeIM #-}

-- | Rebuild the e-graph to restore invariants.
rebuildIM :: forall k a (l :: k -> Type -> Type).
             (LanguageI l, SOrd k, AnalysisI a l)
          => EGraphIM a l ()
rebuildIM = modify rebuildI
{-# INLINE rebuildIM #-}

-- | Find the canonical representative of an e-class.
findIM :: ClassId -> EGraphIM a l ClassId
findIM cid = gets (`findI` cid)
{-# INLINE findIM #-}

-- | Canonicalize an indexed e-node in the state monad.
canonicalizeIM :: forall k a (l :: k -> Type -> Type) dom.
                  (Functor (l dom))
               => l dom ClassId
               -> EGraphIM a l (l dom ClassId)
canonicalizeIM node = gets (`canonicalizeI` node)
{-# INLINE canonicalizeIM #-}

-- | Get the current e-graph.
getEGraphIM :: EGraphIM a l (EGraphI a l)
getEGraphIM = get
{-# INLINE getEGraphIM #-}

-- | Modify the e-graph with a function.
modifyEGraphIM :: (EGraphI a l -> EGraphI a l) -> EGraphIM a l ()
modifyEGraphIM = modify
{-# INLINE modifyEGraphIM #-}
