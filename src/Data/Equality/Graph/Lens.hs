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
import Data.Monoid

import Data.Equality.Utils.SizedList
import Data.Equality.Graph.Internal
import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes
import Data.Equality.Graph.Classes
import Data.Equality.Graph.ReprUnionFind

-- | A 'Lens'' as defined in lens
type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)
-- | A 'Lens' as defined in lens
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
-- | A 'Traversal' as defined in lens
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

-- outdated comment for "getClass":
--
-- Get an e-class from an e-graph given its e-class id
--
-- Returns the canonical id of the class and the class itself
--
-- We'll find its canonical representation and then get it from the e-classes map
--
-- Invariant: The e-class exists.

-- | Lens for the e-class at the representative of the given id in an e-graph
--
-- Calls 'error' when the e-class doesn't exist
_class :: ClassId -> Lens' (EGraph a l) (EClass a l)
_class i afa s =
    let canon_id = findRepr i (unionFind s)
     in (\c' -> s { classes = IM.insert canon_id c' (classes s) }) <$> afa (classes s IM.! canon_id)
{-# INLINE _class #-}

-- | Lens for the memo of e-nodes in an e-graph, that is, a mapping from
-- e-nodes to the e-class they're represented in
_memo :: Lens' (EGraph a l) (NodeMap l ClassId)
_memo afa egr = (\m1 -> egr {memo = m1}) <$> afa (memo egr)
{-# INLINE _memo #-}

-- | Traversal for the existing classes in an e-graph
_classes :: Traversal (EGraph a l) (EGraph b l) (EClass a l) (EClass b l)
_classes afb egr = (\m1 -> egr {classes = m1}) <$> traverse afb (classes egr)
{-# INLINE _classes #-}

-- | Traversal for the existing classes in an e-graph
_iclasses :: Traversal (EGraph a l) (EGraph b l) (ClassId, EClass a l) (EClass b l)
_iclasses afb egr = (\m1 -> egr {classes = m1}) <$> IM.traverseWithKey (curry afb) (classes egr)
{-# INLINE _iclasses #-}

-- | Lens for the 'Domain' of an e-class
_data :: Lens (EClass domain l) (EClass domain' l) domain domain'
_data afb EClass{..} = (\d1 -> EClass eClassId eClassNodes d1 eClassParents) <$> afb eClassData
{-# INLINE _data #-}

-- | Lens for the parent e-classes of an e-class
_parents :: Lens' (EClass a l) (SList (ClassId, ENode l))
_parents afa EClass{..} = EClass eClassId eClassNodes eClassData <$> afa eClassParents
{-# INLINE _parents #-}

-- | Lens for the e-nodes in an e-class
_nodes :: Lens' (EClass a l) (S.Set (ENode l))
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
(%~) :: ASetter s t a b -> (a -> b) -> (s -> t)
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
over :: ASetter s t a b -> (a -> b) -> (s -> t)
over ln f = runIdentity . ln (Identity . f)
{-# INLINE over #-}

-- | Basically 'traverse' over a 'Traversal'
traverseOf :: Traversal s t a b -> forall f. Applicative f => (a -> f b) -> s -> f t 
traverseOf t = t
{-# INLINE traverseOf #-}

-- | Returns True if every target of a Traversable satisfies a predicate.
allOf :: Traversal s t a b -> (a -> Bool) -> s -> Bool
allOf trv f = getAll . getConst . trv (Const . All . f)
{-# INLINE allOf #-}

-- * Utilities

-- We need to use 'ASetter' instead of 'Lens' in %~ to ensure type inference can
-- figure out the Functor or Applicative is 'Identity'. Otherwise, we won't be
-- able to use the 'Traversal' to modify something through a 'Lens'.

-- | Used instead of 'Lens' in 'over' and '%~' to ensure one can call those
-- combinators on 'Lens's and 'Traversal's. Essentially, it helps type
-- inference in such function applications
type ASetter s t a b = (a -> Identity b) -> s -> Identity t
