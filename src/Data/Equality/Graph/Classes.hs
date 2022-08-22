{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
   Module for the definition of 'EClass'.
-}
module Data.Equality.Graph.Classes
    ( module Data.Equality.Graph.Classes
    , module Data.Equality.Graph.Classes.Id
    ) where

import qualified Data.Set as S

import Data.Functor.Classes

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes

import Data.Equality.Analysis

-- | An e-class (an equivalence class of terms) of a language @l@.
--
-- Intuitively, an e-graph is a set of equivalence classes (e-classes). Each
-- e-class is a set of e-nodes representing equivalent terms from a given
-- language, and an e-node is a function symbol paired with a list of children
-- e-classes.
data EClass l = EClass
    { eClassId      :: {-# UNPACK #-} !ClassId -- ^ E-class identifier
    , eClassNodes   :: !(S.Set (ENode l))      -- ^ E-nodes in this class
    , eClassData    :: Domain l                -- ^ The analysis data associated with this eclass.
    , eClassParents :: !(NodeMap l ClassId)    -- ^ E-nodes which are parents of this e-class and their corresponding e-class ids. We found a mapping from nodes to e-class ids a better representation than @[(ENode l, ClassId)]@, and we get de-duplication built-in.
    }

instance (Show (Domain l), Show1 l) => Show (EClass l) where
    show (EClass a b d c) = "Id: " <> show a <> "\nNodes: " <> show b <> "\nParents: " <> show c <> "\nData: " <> show d

