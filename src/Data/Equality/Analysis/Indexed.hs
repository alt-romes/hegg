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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Data.Equality.Analysis.Indexed
Description : E-class analysis for type-indexed expressions
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides the 'AnalysisI' typeclass for type-indexed expressions,
analogous to the 'Analysis' typeclass but preserving type index information.

The key insight is that 'makeAI' receives the full type-indexed expression
with its type indices, allowing type-aware analysis computation.

Example:

@
-- Constant folding analysis that respects types
instance AnalysisI (Maybe Int) TExpr where
    makeAI :: TExpr cods dom (Maybe Int) -> Maybe Int
    makeAI (TConst n) = Just n
    makeAI (TAdd mx my) = liftA2 (+) mx my
    makeAI _ = Nothing

    joinAI = (<|>)
@
-}
module Data.Equality.Analysis.Indexed
    ( -- * Type-indexed analysis
      AnalysisI(..)
    ) where

import Data.Kind (Type)
import Data.Proxy

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Internal (EGraph)
import Data.Equality.Utils.Singleton
import Data.Equality.Utils.Untyped
import Data.Equality.Analysis
import Data.Equality.Language.Indexed

-- | An e-class analysis with domain @domain@ defined for a type-indexed
-- language @l@.
--
-- This is analogous to 'Analysis' but preserves type index information,
-- allowing analysis to be computed based on the specific type of each
-- expression.
--
-- The @domain@ is the type of the analysis data stored in each e-class.
class Eq domain => AnalysisI domain (l :: k -> Type -> Type) where

    -- | When a new e-node is added into a new, singleton e-class, construct
    -- a new value of the domain to be associated with the new e-class.
    --
    -- Unlike 'makeA', this function receives the full type-indexed expression
    -- including type index @dom@, allowing type-aware analysis.
    --
    -- The argument is the e-node term populated with its children data.
    makeAI :: forall dom. SingI dom
           => l dom domain -> domain

    -- | When e-classes c1 and c2 are being merged into c, join d_c1 and
    -- d_c2 into a new value d_c to be associated with the new e-class c.
    --
    -- The 'Proxy' parameter is used to disambiguate which language this
    -- analysis is for, since 'joinAI' doesn't otherwise mention @l@.
    joinAI :: Proxy l -> domain -> domain -> domain

    -- | Optionally modify the e-class c (based on d_c), typically by adding
    -- an e-node to c.
    --
    -- This operates on the erased e-graph since modifications may need to
    -- add new expressions of potentially different types.
    --
    -- Modify should be idempotent if no other changes occur to the e-class,
    -- i.e., @modifyAI(modifyAI(c)) = modifyAI(c)@.
    --
    -- The 'Proxy' parameter disambiguates the language type.
    modifyAI :: Proxy l
             -> ClassId
             -> EGraph domain (ErasedLang l)
             -> EGraph domain (ErasedLang l)
    modifyAI _ _ = id
    {-# INLINE modifyAI #-}

-- | The simplest analysis for type-indexed languages that defines the domain
-- to be @()@ and does nothing otherwise.
instance forall k (l :: k -> Type -> Type). AnalysisI () l where
    makeAI _ = ()
    joinAI _ = (<>)
    {-# INLINE makeAI #-}
    {-# INLINE joinAI #-}

-- | Bridge instance that allows 'AnalysisI' to work with the standard
-- e-graph machinery through 'ErasedLang'.
--
-- This instance delegates to 'AnalysisI' methods, unwrapping the erased
-- expression to access the underlying type-indexed expression.
--
-- NOTE: This is technically an orphan instance (neither the typeclass nor
-- the type is defined in this module), but it's the most logical place for it
-- since it bridges between AnalysisI and Analysis. The orphan warning is
-- suppressed via OPTIONS_GHC pragma at the top of this module.
instance {-# OVERLAPPING #-} (LanguageI l, SOrd k, AnalysisI domain l)
         => Analysis domain (ErasedLang (l :: k -> Type -> Type)) where

    makeA :: ErasedLang l domain -> domain
    makeA (ErasedLang (Untyped x)) = makeAI x
    {-# INLINE makeA #-}

    joinA :: domain -> domain -> domain
    joinA = joinAI (Proxy :: Proxy l)
    {-# INLINE joinA #-}

    modifyA :: ClassId -> EGraph domain (ErasedLang l) -> EGraph domain (ErasedLang l)
    modifyA = modifyAI (Proxy :: Proxy l)
    {-# INLINE modifyA #-}
