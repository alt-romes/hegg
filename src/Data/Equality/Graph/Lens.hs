{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-|
  Hand-rolled lenses on e-graphs and e-classes which come in quite handy, are
  heavily used in 'Data.Equality.Graph', and are the only exported way of
  editing the structure of the e-graph. If you want to write some complex
  'Analysis' you'll probably need these.
 -}
module Data.Equality.Graph.Lens where

import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Data.Functor.Identity
import Data.Functor.Const

import Data.Equality.Graph.Internal
import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes
import Data.Equality.Graph.Classes
import Data.Equality.Graph.ReprUnionFind
import Data.Equality.Analysis

-- | A 'Lens'' as defined in other lenses libraries
type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)


-- outdated comment for "getClass":
--
-- Get an e-class from an e-graph given its e-class id
--
-- Returns the canonical id of the class and the class itself
--
-- We'll find its canonical representation and then get it from the e-classes map
--
-- Invariant: The e-class exists.

-- | Lens for the e-class with the given id in an e-graph
--
-- Calls 'error' when the e-class doesn't exist
_class :: ClassId -> Lens' (EGraph l) (EClass l)
_class i afa s =
    let canon_id = findRepr i (unionFind s)
     in (\c' -> s { classes = IM.insert canon_id c' (classes s) }) <$> afa (classes s IM.! canon_id)
{-# INLINE _class #-}

-- | Lens for the memo of e-nodes in an e-graph, that is, a mapping from
-- e-nodes to the e-class they're represented in
_memo :: Lens' (EGraph l) (NodeMap l ClassId)
_memo afa egr = (\m1 -> egr {memo = m1}) <$> afa (memo egr)
{-# INLINE _memo #-}

-- | Lens for the map of existing classes by id in an e-graph
_classes :: Lens' (EGraph l) (ClassIdMap (EClass l))
_classes afa egr = (\m1 -> egr {classes = m1}) <$> afa (classes egr)
{-# INLINE _classes #-}

-- | Lens for the 'Domain' of an e-class
_data :: Lens' (EClass l) (Domain l)
_data afa EClass{..} = (\d1 -> EClass eClassId eClassNodes d1 eClassParents) <$> afa eClassData
{-# INLINE _data #-}

-- | Lens for the parent e-classes of an e-class
_parents :: Lens' (EClass l) (NodeMap l ClassId)
_parents afa EClass{..} = EClass eClassId eClassNodes eClassData <$> afa eClassParents
{-# INLINE _parents #-}

-- | Lens for the e-nodes in an e-class
_nodes :: Lens' (EClass l) (S.Set (ENode l))
_nodes afa EClass{..} = (\ns -> EClass eClassId ns eClassData eClassParents) <$> afa eClassNodes
{-# INLINE _nodes #-}

-- | Like @'view'@ but with the arguments flipped
(^.) :: s -> Lens' s a -> a
(^.) s ln = view ln s
infixl 8 ^.
{-# INLINE (^.) #-}

-- | Synonym for @'set'@
(.~) :: Lens' s a -> a -> (s -> s)
(.~) = set
infixr 4 .~
{-# INLINE (.~) #-}

-- | Synonym for @'over'@
(%~) :: Lens' s a -> (a -> a) -> (s -> s)
(%~) = over
infixr 4 %~
{-# INLINE (%~) #-}

-- | Applies a getter to a value
view :: Lens' s a -> (s -> a)
view ln = getConst . ln Const
{-# INLINE view #-}

-- | Applies a setter to a value
set :: Lens' s a -> a -> (s -> s)
set ln x = over ln (const x)
{-# INLINE set #-}

-- | Applies a function to the target
over :: Lens' s a -> (a -> a) -> (s -> s)
over ln f = runIdentity . ln (Identity . f)
{-# INLINE over #-}

