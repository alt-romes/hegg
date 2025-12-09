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
Module      : Data.Equality.Matching.Pattern.Indexed
Description : Type-indexed patterns for type-safe e-matching
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides type-indexed patterns that preserve type information
while supporting conversion to standard patterns for e-matching.

A 'PatternI' carries type index that matches the corresponding indexed
language, ensuring patterns are well-typed at compile time.

Example:

@
-- A type-indexed pattern for TExpr
pattern1 :: PatternI TExpr 'TyInt
pattern1 = patI (TAdd \"x\" \"y\")

-- Erase to use with standard e-matching
erasedPat :: Pattern (ErasedLang TExpr)
erasedPat = erasePatternI pattern1
@
-}
module Data.Equality.Matching.Pattern.Indexed
    ( -- * Type-indexed patterns
      PatternI(..)
      -- * Smart constructors
    , patI
      -- * Erasure
    , erasePatternI
    ) where

import Data.Kind (Type)
import Data.String (IsString(..))

import Data.Equality.Matching.Pattern (Pattern(..))
import Data.Equality.Utils.Singleton
import Data.Equality.Utils.Untyped
import Data.Equality.Language.Indexed

-- | A type-indexed pattern that preserves type information.
--
-- @PatternI l dom@ is a pattern for expressions of type @l dom@,
-- where @dom@ is the result type.
--
-- * 'NonVariablePatternI' matches a specific expression constructor
-- * 'VariablePatternI' matches any expression (wildcard)
data PatternI (l :: k -> Type -> Type) (dom :: k) where
    -- | Pattern matching a specific constructor.
    -- The children are patterns themselves.
    NonVariablePatternI :: SingI dom
                        => l dom (PatternI l dom')
                        -> PatternI l dom
    -- | Variable pattern (wildcard) that matches any expression.
    -- Variables are identified by strings for named matching.
    VariablePatternI :: SingI dom
                     => String
                     -> PatternI l dom

-- | Smart constructor for 'NonVariablePatternI'.
--
-- Example:
--
-- @
-- patI (TAdd \"x\" \"y\") :: PatternI TExpr 'TyInt
-- @
patI :: SingI dom
     => l dom (PatternI l dom')
     -> PatternI l dom
patI = NonVariablePatternI
{-# INLINE patI #-}

-- | 'IsString' instance allows using string literals for variable patterns.
--
-- With @OverloadedStrings@, you can write:
--
-- @
-- \"x\" :: PatternI l dom
-- @
instance SingI dom => IsString (PatternI l dom) where
    fromString = VariablePatternI
    {-# INLINE fromString #-}

-- | Convert a type-indexed pattern to a standard pattern over 'ErasedLang'.
--
-- This allows type-indexed patterns to be used with the existing
-- e-matching infrastructure.
--
-- The conversion wraps each pattern node in 'Untyped' and 'ErasedLang',
-- preserving the structure while erasing type index.
erasePatternI :: forall k (l :: k -> Type -> Type) dom.
                 (Functor (l dom), LanguageI l, SOrd k)
              => PatternI l dom
              -> Pattern (ErasedLang l)
erasePatternI (VariablePatternI s) = VariablePattern s
erasePatternI (NonVariablePatternI node) =
    NonVariablePattern (ErasedLang (Untyped (fmap erasePatternI' node)))
  where
    -- Helper for recursive erasure without requiring Functor constraint
    erasePatternI' :: forall dom'. PatternI l dom' -> Pattern (ErasedLang l)
    erasePatternI' (VariablePatternI s) = VariablePattern s
    erasePatternI' (NonVariablePatternI n) =
        NonVariablePattern (ErasedLang (Untyped (fmap erasePatternI' n)))
{-# INLINE erasePatternI #-}

-- | Eq instance compares via erasure.
--
-- Two patterns are equal if their erased forms are equal.
instance (LanguageI l, SOrd k,
          forall dom'. Functor (l dom'),
          forall a. Eq a => Eq (ErasedLang l a))
         => Eq (PatternI (l :: k -> Type -> Type) dom) where
    p1 == p2 = erasePatternI p1 == erasePatternI p2
    {-# INLINE (==) #-}

-- | Show instance displays pattern structure.
instance (forall dom' a. Show a => Show (l dom' a))
         => Show (PatternI l dom) where
    showsPrec _ (VariablePatternI s) = showString (show s)
    showsPrec d (NonVariablePatternI x) = showsPrec d x
