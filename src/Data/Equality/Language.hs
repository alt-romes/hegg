{-# LANGUAGE FlexibleContexts #-}
module Data.Equality.Language
    ( module Data.Equality.Language
    , module Data.Equality.Analysis
    ) where

import Data.Functor.Classes

import Data.Equality.Analysis

-- | A language is a recursive data type written in its functor \"form\"
--
-- Must satisfy all other class constraints
class (Analysis l, Traversable l, Ord1 l) => Language l where

