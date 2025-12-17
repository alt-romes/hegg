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
Module      : Data.Equality.Matching.Indexed
Description : Type-indexed e-matching
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides type-indexed e-matching that works with type-indexed
patterns and e-graphs while delegating to the standard e-matching infrastructure.

The key insight is that we can erase type index for matching and then
provide a typed interface over the results.

Example:

@
-- Create a typed pattern
pattern1 :: PatternI TExpr 'TyInt
pattern1 = patI (TAdd \"x\" \"y\")

-- Match against an indexed e-graph
matches = ematchI egraphI pattern1
@
-}
module Data.Equality.Matching.Indexed
    ( -- * Type-indexed e-matching
      ematchI
      -- * Database conversion
    , eGraphToDatabaseI
      -- * Query compilation
    , compileToQueryI
      -- * Re-exports
    , Match(..)
    , Database
    , Query
    , VarsState(..)
    , findVarName
    ) where

import Data.Kind (Type)

import Data.Equality.Graph.Indexed
import Data.Equality.Matching
import Data.Equality.Matching.Database (Database, Query, Var)
import Data.Equality.Matching.Pattern.Indexed
import Data.Equality.Language.Indexed
import Data.Equality.Utils.Singleton
import Data.Equality.Utils.Untyped

-- | Match a type-indexed pattern against an indexed e-graph.
--
-- This function erases the type index from the pattern and e-graph,
-- performs standard e-matching, and returns the matches.
--
-- The returned 'Match' contains substitutions mapping pattern variable
-- names to e-class IDs.
--
-- Example:
--
-- @
-- let pat = patI (TAdd \"x\" \"y\") :: PatternI TExpr 'TyInt
-- let matches = ematchI db pat
-- -- Each match contains a substitution for \"x\" and \"y\"
-- @
ematchI :: forall k (l :: k -> Type -> Type) dom.
           (LanguageI l, SOrd k, Functor (l dom))
        => Database (ErasedLang l)
        -> PatternI l dom
        -> [Match]
ematchI db patI' =
    let erasedPat = erasePatternI patI'
        (query, root) = fst $ compileToQuery erasedPat
    in ematch db (query, root)
{-# INLINE ematchI #-}

-- | Convert an indexed e-graph to a database for e-matching.
--
-- The database can be reused across multiple pattern matches on the
-- same e-graph state.
--
-- Example:
--
-- @
-- let db = eGraphToDatabaseI egraph
-- let matches1 = ematchI db pattern1
-- let matches2 = ematchI db pattern2
-- @
eGraphToDatabaseI :: forall k a (l :: k -> Type -> Type).
                     (LanguageI l, SOrd k)
                  => EGraphI a l
                  -> Database (ErasedLang l)
eGraphToDatabaseI egi =
    case erasedIsLanguage @k @l of
        Dict -> eGraphToDatabase (getEGraphI egi)
{-# INLINE eGraphToDatabaseI #-}

-- | Compile a type-indexed pattern to a query.
--
-- Returns the query and root variable along with the variable state
-- mapping pattern variable names to internal variables.
--
-- This is useful when you need access to the variable mappings for
-- interpreting match results.
compileToQueryI :: forall k (l :: k -> Type -> Type) dom.
                   (LanguageI l, SOrd k, Functor (l dom))
                => PatternI l dom
                -> ((Query (ErasedLang l), Var), VarsState)
compileToQueryI patI' = compileToQuery (erasePatternI patI')
{-# INLINE compileToQueryI #-}
