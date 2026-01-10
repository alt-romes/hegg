{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Data.Equality.Extraction.Indexed
Description : Extraction from type-indexed e-graphs
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides extraction functions for type-indexed e-graphs.

Since equality saturation may combine expressions of different types
within an equivalence class, extraction returns the type-erased
representation. Users can then attempt to reconstruct typed expressions
from the erased form if needed.

Example:

@
-- Extract best expression from indexed e-graph
let best = extractBestI egraphI costFn classId

-- The result is in erased form: Fix (ErasedLang TExpr)
@
-}
module Data.Equality.Extraction.Indexed
    ( -- * Extraction
      extractBestI
      -- * Cost functions
    , CostFunction
    , depthCostI
      -- * Re-exports
    , Fix(..)
    , ClassId
    ) where

import Data.Kind (Type)

import Data.Equality.Utils (Fix(..))
import Data.Equality.Graph (ClassId)
import Data.Equality.Graph.Indexed
import Data.Equality.Extraction (CostFunction, extractBest, depthCost)
import Data.Equality.Language.Indexed
import Data.Equality.Utils.Singleton
import Data.Equality.Utils.Untyped (ErasedLang)

-- | Extract the best expression from an equivalence class in an indexed e-graph.
--
-- The result is in erased form since equality saturation may combine
-- expressions of different types. Users can attempt to reconstruct
-- typed expressions from the erased form if needed.
--
-- Example:
--
-- @
-- let (classId, egraph) = addI (TConst 42) emptyEGraphI
-- let best = extractBestI egraph depthCostI classId
-- @
extractBestI :: forall k a (l :: k -> Type -> Type) cost.
                (LanguageI l, SOrd k, Ord cost)
             => EGraphI a l
             -> CostFunction (ErasedLang l) cost
             -> ClassId
             -> Fix (ErasedLang l)
extractBestI (EGraphI eg) cost classId =
    case erasedIsLanguage @k @l of
        Dict -> extractBest eg cost classId
{-# INLINE extractBestI #-}

-- | Simple depth-based cost function for indexed languages.
--
-- The deeper the expression tree, the higher the cost.
-- This is useful as a default cost function when you want
-- simpler (shallower) expressions.
depthCostI :: forall k (l :: k -> Type -> Type).
              (LanguageI l, SOrd k)
           => CostFunction (ErasedLang l) Int
depthCostI = case erasedIsLanguage @k @l of
    Dict -> depthCost
{-# INLINE depthCostI #-}
