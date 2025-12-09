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
Module      : Data.Equality.Saturation.Rewrites.Indexed
Description : Type-preserving rewrites for type-indexed expressions
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides type-indexed rewrites that enforce type preservation
at compile time. The left-hand side and right-hand side of a rewrite must
have matching type index, ensuring type-safe transformations.

A key property is that attempting to create a rewrite between expressions
of different types will result in a compile-time type error.

Example:

@
-- Type-preserving commutativity rule (compiles)
commute :: RewriteI () TExpr 'TyInt
commute = patI (TAdd \"x\" \"y\") :=: patI (TAdd \"y\" \"x\")

-- Ill-typed rule (does NOT compile - type mismatch!)
-- badRule = patI (TConst 1) :=: patI (TBool True)
@
-}
module Data.Equality.Saturation.Rewrites.Indexed
    ( -- * Type-indexed rewrites
      RewriteI(..)
      -- * Existential wrapper
    , SomeRewrite(..)
      -- * Smart constructors
    , ruleI
      -- * Rewrite conditions
    , RewriteConditionI
      -- * Erasure
    , eraseRewriteI
    , eraseSomeRewrite
    ) where

import Data.Kind (Type)

import Data.Equality.Graph (EGraph)
import Data.Equality.Matching (VarsState)
import Data.Equality.Matching.Database (Subst)
import Data.Equality.Matching.Pattern.Indexed
import Data.Equality.Saturation.Rewrites (Rewrite(..))
import Data.Equality.Utils.Singleton
import Data.Equality.Utils.Untyped
import Data.Equality.Language.Indexed

-- | A type-indexed rewrite rule that enforces type preservation.
--
-- Both the left-hand side and right-hand side patterns must have the same
-- type index @dom@, ensuring the rewrite preserves types.
--
-- * @:=:@ creates a simple rewrite rule
-- * @:|:@ adds a condition to a rewrite rule
data RewriteI anl (l :: k -> Type -> Type) (dom :: k) where
    -- | A simple rewrite from LHS pattern to RHS pattern.
    -- Type index must match, enforcing type preservation.
    (:=:) :: SingI dom
          => !(PatternI l dom)
          -> !(PatternI l dom)
          -> RewriteI anl l dom

    -- | A conditional rewrite that only applies when the condition is met.
    (:|:) :: !(RewriteI anl l dom)
          -> !(RewriteConditionI anl l)
          -> RewriteI anl l dom

infix 3 :=:
infixl 2 :|:

-- | A condition for applying a rewrite rule.
--
-- Takes the variable state, substitution, and e-graph, returning whether
-- the condition is satisfied.
type RewriteConditionI anl l = VarsState -> Subst -> EGraph anl (ErasedLang l) -> Bool

-- | Existential wrapper for type-indexed rewrites.
--
-- This allows collecting rewrites with different type indices into
-- a heterogeneous collection (e.g., a list).
--
-- The singleton witness @SingI@ is preserved,
-- allowing type recovery when needed.
data SomeRewrite anl (l :: k -> Type -> Type) where
    SomeRewrite :: SingI dom
                => RewriteI anl l dom
                -> SomeRewrite anl l

-- | Smart constructor for creating a 'SomeRewrite' from two patterns.
--
-- Example:
--
-- @
-- commuteRule :: SomeRewrite () TExpr
-- commuteRule = ruleI (patI (TAdd \"x\" \"y\")) (patI (TAdd \"y\" \"x\"))
-- @
ruleI :: SingI dom
      => PatternI l dom
      -> PatternI l dom
      -> SomeRewrite anl l
ruleI lhs rhs = SomeRewrite (lhs :=: rhs)
{-# INLINE ruleI #-}

-- | Erase a type-indexed rewrite to a standard rewrite over 'ErasedLang'.
--
-- This allows type-indexed rewrites to be used with the existing
-- equality saturation infrastructure.
eraseRewriteI :: forall k anl (l :: k -> Type -> Type) dom.
                 (LanguageI l, SOrd k, forall dom'. Functor (l dom'))
              => RewriteI anl l dom
              -> Rewrite anl (ErasedLang l)
eraseRewriteI (lhs :=: rhs) = erasePatternI lhs := erasePatternI rhs
eraseRewriteI (rw :|: cond) = eraseRewriteI rw :| cond
{-# INLINE eraseRewriteI #-}

-- | Erase a 'SomeRewrite' to a standard rewrite.
eraseSomeRewrite :: forall k anl (l :: k -> Type -> Type).
                    (LanguageI l, SOrd k, forall dom. Functor (l dom))
                 => SomeRewrite anl l
                 -> Rewrite anl (ErasedLang l)
eraseSomeRewrite (SomeRewrite rw) = eraseRewriteI rw
{-# INLINE eraseSomeRewrite #-}

-- | Show instance for debugging.
instance (forall dom' a. Show a => Show (l dom' a))
         => Show (RewriteI anl l dom) where
    show (lhs :=: rhs) = show lhs ++ " :=: " ++ show rhs
    show (rw :|: _) = show rw ++ " :|: <cond>"

-- | Show instance for SomeRewrite.
instance (forall dom a. Show a => Show (l dom a))
         => Show (SomeRewrite anl l) where
    show (SomeRewrite rw) = show rw
