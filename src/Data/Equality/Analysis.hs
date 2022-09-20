{-# LANGUAGE AllowAmbiguousTypes #-} -- joinA
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|

E-class analysis, which allows the concise expression of a program analysis over
the e-graph.

An e-class analysis resembles abstract interpretation lifted to the e-graph
level, attaching analysis data from a semilattice to each e-class.

The e-graph maintains and propagates this data as e-classes get merged and new
e-nodes are added.

Analysis data can be used directly to modify the e-graph, to inform how or if
rewrites apply their right-hand sides, or to determine the cost of terms during
the extraction process.

References: https://arxiv.org/pdf/2004.03082.pdf

-}
module Data.Equality.Analysis where

import Data.Kind (Type)

import Data.Equality.Utils

import {-# SOURCE #-} Data.Equality.Graph.Classes (EClass)


-- | TODO: If the domain is used in the class definition we would no longer have
-- ambiguous types and the type family. That is, we'd have @Analysis domain
-- language@. Would that be better?
--
-- | The e-class analysis defined for a language @l@.
class Eq (Domain a l) => Analysis a (l :: Type -> Type) where

    -- | Domain of data stored in e-class according to e-class analysis
    type Domain a l

    -- | When a new e-node is added into a new, singleton e-class, construct a
    -- new value of the domain to be associated with the new e-class, typically
    -- (always?) by accessing the associated data of n's children
    --
    -- The argument is the e-node term populated with its children data
    --
    -- === Example
    --
    -- @
    -- -- (Domain a l) = Maybe Double
    -- makeA :: Expr (Maybe Double) -> Maybe Double
    -- makeA = \case
    --     BinOp Div e1 e2 -> liftA2 (/) e1 e2
    --     BinOp Sub e1 e2 -> liftA2 (-) e1 e2
    --     BinOp Mul e1 e2 -> liftA2 (*) e1 e2
    --     BinOp Add e1 e2 -> liftA2 (+) e1 e2
    --     Const x -> Just x
    --     Sym _ -> Nothing
    -- @
    makeA :: l (Domain a l) -> Domain a l

    -- | When e-classes c1 c2 are being merged into c, join d_c1 and
    -- d_c2 into a new value d_c to be associated with the new
    -- e-class c
    joinA :: Domain a l -> Domain a l -> Domain a l

    -- | Optionally modify the e-class c (based on d_c), typically by adding an
    -- e-node to c. Modify should be idempotent if no other changes occur to
    -- the e-class, i.e., modify(modify(c)) = modify(c)
    --
    -- The return value of the modify function is both the modified class and
    -- the expressions (in their fixed-point form) to add to this class. We
    -- can't manually add them because not only would it skip some of the
    -- internal steps of representing + merging, but also because it's
    -- impossible to add any expression with depth > 0 without access to the
    -- e-graph (since we must represent every sub-expression in the e-graph
    -- first).
    --
    -- That's why we must return the modified class and the expressions to add
    -- to this class.
    --
    -- === Example
    --
    -- Pruning an e-class with a constant value of all its nodes except for the
    -- leaf values, and adding a constant value node
    --
    -- @
    --  -- Prune all except leaf e-nodes
    --  modifyA cl =
    --    case cl^._data of
    --      Nothing -> (cl, [])
    --      Just d -> ((_nodes %~ S.filter (F.null .unNode)) cl, [Fix (Const d)])
    -- @
    modifyA :: EClass a l -> (EClass a l, [Fix l])
    modifyA c = (c, [])
    {-# INLINE modifyA #-}


-- | The simplest analysis that defines the domain to be () and does nothing
-- otherwise
instance forall l. Analysis () l where
  type Domain () _ = ()
  makeA _ = ()
  joinA = (<>)
