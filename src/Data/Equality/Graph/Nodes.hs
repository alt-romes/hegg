{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-|

Definition of e-nodes, instances and some operations on them.

Additionally, defines the 'Operator' of an e-node as the language functor
parametrised over @()@.

-}
module Data.Equality.Graph.Nodes where

import Data.Functor.Classes
import Data.Foldable
import Data.Kind

import Control.Monad (void)

import Data.Equality.Graph.Classes.Id

-- | E-node
--
-- An E-node is a function symbol paired with a list of children e-classes.
-- 
-- We define an e-node to be the base functor of some recursive data type
-- parametrized over ClassId, i.e. all recursive fields are rather e-class ids.
--
-- When @l@ is an expression-like data type, @ENode l = l ClassId@ means every
-- recursive field (so, every argument passed to this expr) is a 'ClassId'
-- rather than an explicit expression
--
-- An e-node carries a type indicating whether the node is canonicalized or
-- not. The 'Memo' structure requires that e-nodes prove to be canonicalized on
-- lookup and insertion
type ENode :: Canonicality -> (Type -> Type) -> Type
newtype ENode k l = Node { unNode :: l (ClassId' k) }

-- | Operator
--
-- An operator is solely the function symbol part of the e-node, that is,
-- children e-classes are ignored.
newtype Operator l = Operator { unOperator :: l () }

-- | Get the children class ids of an e-node
children :: Traversable l => ENode k l -> [ClassId' k]
children = toList . unNode
{-# INLINE children #-}

-- | Get the operator (function symbol) of an e-node
operator :: Traversable l => ENode k l -> Operator l
operator = Operator . void . unNode
{-# INLINE operator #-}

loseNodeCanon :: Functor l => ENode k1 l -> ENode k2 l
loseNodeCanon = \case
    Node l -> Node (fmap (ClassId . unwrapId) l)
{-# INLINE loseNodeCanon #-}


-- Instances

instance Eq1 l => (Eq (ENode k l)) where
    (==) (Node a) (Node b) = liftEq (==) a b

instance Ord1 l => (Ord (ENode k l)) where
    compare (Node a) (Node b) = liftCompare compare a b

instance Show1 l => (Show (ENode k l)) where
    showsPrec p (Node l) = liftShowsPrec showsPrec showList p l

instance Eq1 l => (Eq (Operator l)) where
    (==) (Operator a) (Operator b) = liftEq (==) a b

instance Ord1 l => (Ord (Operator l)) where
    compare (Operator a) (Operator b) = liftCompare compare a b

instance Show1 l => (Show (Operator l)) where
    showsPrec p (Operator l) = liftShowsPrec (const . const $ showString "") (const $ showString "") p l
