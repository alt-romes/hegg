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

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes

import Data.Equality.Utils.SizedList

-- | An e-class (an equivalence class of terms) of a language @l@.
--
-- Intuitively, an e-graph is a set of equivalence classes (e-classes). Each
-- e-class is a set of e-nodes representing equivalent terms from a given
-- language, and an e-node is a function symbol paired with a list of children
-- e-classes.
data EClass analysis_domain language = EClass
    { eClassId      :: {-# UNPACK #-} !ClassId -- ^ E-class identifier
    , eClassNodes   :: !(S.Set (ENode language))      -- ^ E-nodes in this class
    , eClassData    :: analysis_domain                       -- ^ The analysis data associated with this eclass.
    , eClassParents :: !(SList (ClassId, ENode language)) -- ^ E-nodes which are parents of this e-class and their corresponding e-class ids.
    }

instance (Show a, Show (l ClassId)) => Show (EClass a l) where
    show (EClass a b d (SList c _)) = "Id: " <> show a <> "\nNodes: " <> show b <> "\nParents: " <> show c <> "\nData: " <> show d

