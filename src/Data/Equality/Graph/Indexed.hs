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
{-# LANGUAGE IncoherentInstances #-}
{-|
Module      : Data.Equality.Graph.Indexed
Description : Type-indexed e-graph wrapper
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides a type-indexed wrapper around the standard e-graph that
maintains type safety for type-indexed expression languages.

The key insight is that while the underlying e-graph stores type-erased
expressions via 'ErasedLang', the typed interface ensures that expressions
are correctly typed when added and can be extracted with their types.

Example usage:

@
-- Create empty indexed e-graph
let eg = emptyEGraphI :: EGraphI () TExpr

-- Add typed expressions
let c1 = TConst 1 :: TExpr '[] 'TyInt ClassId
    (id1, eg1) = addI c1 eg

-- Merge and rebuild work as normal
let (merged, eg2) = mergeI id1 id1 eg1
    eg3 = rebuildI eg2
@
-}
module Data.Equality.Graph.Indexed
    ( -- * Indexed E-Graph type
      EGraphI(..)
      -- * Construction
    , emptyEGraphI
      -- * E-graph operations
    , addI
    , mergeI
    , rebuildI
      -- * Querying
    , findI
    , canonicalizeI
    ) where

import Data.Kind (Type)

import Data.Equality.Graph (EGraph, ClassId, ENode(..))
import qualified Data.Equality.Graph as EG
import Data.Equality.Language.Indexed
import Data.Equality.Analysis.Indexed
import Data.Equality.Utils.Singleton
import Data.Equality.Utils.Untyped

-- | Type-indexed e-graph wrapper.
--
-- This newtype wraps an 'EGraph' over 'ErasedLang' to provide type-safe
-- operations for type-indexed expression languages.
--
-- The type parameters are:
--
-- * @a@ - The analysis domain
-- * @l@ - The type-indexed language functor with kind @k -> Type -> Type@
newtype EGraphI a (l :: k -> Type -> Type) = EGraphI
    { getEGraphI :: EGraph a (ErasedLang l)
      -- ^ Access the underlying untyped e-graph
    }

-- | Create an empty indexed e-graph.
emptyEGraphI :: forall k a (l :: k -> Type -> Type).
                (LanguageI l, SOrd k)
             => EGraphI a l
emptyEGraphI = case erasedIsLanguage @k @l of
    Dict -> EGraphI EG.emptyEGraph
{-# INLINE emptyEGraphI #-}

-- | Add a type-indexed e-node to the e-graph.
--
-- The e-node's children should be 'ClassId's referencing existing e-classes.
-- Returns the class ID of the (possibly new) e-class containing the e-node.
--
-- If an equivalent e-node already exists, returns its existing class ID
-- (hashcons behavior).
addI :: forall k a (l :: k -> Type -> Type) dom.
        (LanguageI l, SOrd k, AnalysisI a l, SingI dom)
     => l dom ClassId
     -> EGraphI a l
     -> (ClassId, EGraphI a l)
addI node (EGraphI eg) =
    case erasedIsLanguage @k @l of
        Dict ->
            let erasedNode = Node (ErasedLang (Untyped node))
                (cid, eg') = EG.add erasedNode eg
            in (cid, EGraphI eg')
{-# INLINE addI #-}

-- | Merge two e-classes by their class IDs.
--
-- After merging, both class IDs will refer to the same equivalence class.
-- E-graph invariants may be temporarily broken; call 'rebuildI' to restore them.
mergeI :: forall k a (l :: k -> Type -> Type).
          (LanguageI l, SOrd k, AnalysisI a l)
       => ClassId
       -> ClassId
       -> EGraphI a l
       -> (ClassId, EGraphI a l)
mergeI c1 c2 (EGraphI eg) =
    case erasedIsLanguage @k @l of
        Dict ->
            let (cid, eg') = EG.merge c1 c2 eg
            in (cid, EGraphI eg')
{-# INLINE mergeI #-}

-- | Rebuild the e-graph to restore invariants.
--
-- After performing merges, the e-graph may have broken invariants (deduplication
-- and congruence). This function restores them.
rebuildI :: forall k a (l :: k -> Type -> Type).
            (LanguageI l, SOrd k, AnalysisI a l)
         => EGraphI a l
         -> EGraphI a l
rebuildI (EGraphI eg) =
    case erasedIsLanguage @k @l of
        Dict -> EGraphI (EG.rebuild eg)
{-# INLINE rebuildI #-}

-- | Find the canonical representative of an e-class.
findI :: EGraphI a l -> ClassId -> ClassId
findI (EGraphI eg) cid = EG.find cid eg
{-# INLINE findI #-}

-- | Canonicalize an indexed e-node.
--
-- Replaces all child class IDs with their canonical representatives.
canonicalizeI :: forall k a (l :: k -> Type -> Type) dom.
                 (Functor (l dom))
              => EGraphI a l
              -> l dom ClassId
              -> l dom ClassId
canonicalizeI (EGraphI eg) node = fmap (`EG.find` eg) node
{-# INLINE canonicalizeI #-}
