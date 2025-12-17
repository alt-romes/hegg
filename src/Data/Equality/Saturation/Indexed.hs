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
Module      : Data.Equality.Saturation.Indexed
Description : Type-indexed equality saturation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides type-indexed equality saturation that works with
type-indexed rewrites while leveraging the standard equality saturation
infrastructure via type erasure.

The key insight is that equality saturation operates on the erased
representation, and type-indexed rewrites are converted to standard
rewrites via 'eraseSomeRewrite'.

Example:

@
-- Define type-indexed rewrites
commuteAdd :: SomeRewrite () TExpr
commuteAdd = ruleI (patI (TAdd \"x\" \"y\")) (patI (TAdd \"y\" \"x\"))

-- Run equality saturation on erased expression
(result, finalGraph) = equalitySaturationI erasedExpr [commuteAdd] cost
@
-}
module Data.Equality.Saturation.Indexed
    ( -- * Type-indexed equality saturation
      equalitySaturationI
    , equalitySaturationI'
    , runEqualitySaturationI
      -- * Re-exports
    , SomeRewrite(..)
    , ruleI
    , CostFunction
    , Fix(..)
    ) where

import Data.Kind (Type)

import Control.Monad.Trans.State.Strict (runState)

import Data.Equality.Utils (Fix(..))
import Data.Equality.Graph (EGraph)
import Data.Equality.Graph.Indexed
import Data.Equality.Extraction (CostFunction)
import Data.Equality.Saturation (equalitySaturation', runEqualitySaturation)
import Data.Equality.Saturation.Rewrites (Rewrite)
import Data.Equality.Saturation.Rewrites.Indexed
import Data.Equality.Saturation.Scheduler
import Data.Equality.Language (Language)
import Data.Equality.Language.Indexed
import Data.Equality.Analysis (Analysis)
import Data.Equality.Analysis.Indexed
import Data.Equality.Utils.Singleton
import Data.Equality.Utils.Untyped (ErasedLang)

-- | Equality saturation with defaults for type-indexed rewrites.
--
-- Takes an erased expression, a list of type-indexed rewrites, and a cost
-- function. Returns the best equivalent expression and the resulting e-graph
-- wrapped in the indexed wrapper.
--
-- The expression is given in erased form ('Fix (ErasedLang l)') since
-- equality saturation operates on the type-erased representation internally.
--
-- Example:
--
-- @
-- let commute = ruleI (patI (TAdd \"x\" \"y\")) (patI (TAdd \"y\" \"x\"))
-- let (best, egraph) = equalitySaturationI erasedExpr [commute] costFn
-- @
equalitySaturationI :: forall k a (l :: k -> Type -> Type) cost.
                       ( LanguageI l
                       , SOrd k
                       , AnalysisI a l
                       , Ord cost
                       , forall dom. Functor (l dom)
                       )
                    => Fix (ErasedLang l)              -- ^ Expression to run equality saturation on
                    -> [SomeRewrite a l]               -- ^ List of type-indexed rewrite rules
                    -> CostFunction (ErasedLang l) cost -- ^ Cost function
                    -> (Fix (ErasedLang l), EGraphI a l) -- ^ Best expression and resulting e-graph
equalitySaturationI = equalitySaturationI' defaultBackoffScheduler
{-# INLINE equalitySaturationI #-}

-- | Equality saturation with custom scheduler for type-indexed rewrites.
--
-- This variant allows specifying a custom scheduler for controlling
-- rewrite rule application.
equalitySaturationI' :: forall k a (l :: k -> Type -> Type) schd cost.
                        ( LanguageI l
                        , SOrd k
                        , AnalysisI a l
                        , Scheduler (ErasedLang l) schd
                        , Ord cost
                        , forall dom. Functor (l dom)
                        )
                     => schd                             -- ^ Scheduler to use
                     -> Fix (ErasedLang l)               -- ^ Expression to run equality saturation on
                     -> [SomeRewrite a l]                -- ^ List of type-indexed rewrite rules
                     -> CostFunction (ErasedLang l) cost -- ^ Cost function
                     -> (Fix (ErasedLang l), EGraphI a l) -- ^ Best expression and resulting e-graph
equalitySaturationI' schd expr rewrites cost =
    case erasedIsLanguage @k @l of
        Dict ->
            let erasedRewrites = map eraseSomeRewrite rewrites
                (bestExpr, eg) = equalitySaturation' schd expr erasedRewrites cost
            in (bestExpr, EGraphI eg)
{-# INLINE equalitySaturationI' #-}

-- | Run equality saturation on an indexed e-graph.
--
-- This function applies rewrite rules to an indexed e-graph until
-- saturation or the iteration limit is reached, returning the saturated
-- e-graph.
--
-- Note: This runs saturation on the e-graph without extracting a result.
-- For full equality saturation with extraction, use 'equalitySaturationI'.
runEqualitySaturationI :: forall k a (l :: k -> Type -> Type) schd.
                          ( LanguageI l
                          , SOrd k
                          , AnalysisI a l
                          , Scheduler (ErasedLang l) schd
                          , forall dom. Functor (l dom)
                          )
                       => schd              -- ^ Scheduler to use
                       -> [SomeRewrite a l] -- ^ List of type-indexed rewrite rules
                       -> EGraphI a l       -- ^ E-graph to saturate
                       -> EGraphI a l       -- ^ Saturated e-graph
runEqualitySaturationI schd rewrites (EGraphI eg) =
    case erasedIsLanguage @k @l of
        Dict ->
            let erasedRewrites = map eraseSomeRewrite rewrites
                ((), eg') = runEGraphMSat schd erasedRewrites eg
            in EGraphI eg'
  where
    -- Helper to run saturation in e-graph monad
    runEGraphMSat :: (Analysis a (ErasedLang l), Language (ErasedLang l), Scheduler (ErasedLang l) s)
                  => s
                  -> [Rewrite a (ErasedLang l)]
                  -> EGraph a (ErasedLang l)
                  -> ((), EGraph a (ErasedLang l))
    runEGraphMSat s rws egraph =
        runState (runEqualitySaturation s rws) egraph
{-# INLINE runEqualitySaturationI #-}
