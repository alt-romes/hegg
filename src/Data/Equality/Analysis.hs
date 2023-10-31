{-# LANGUAGE AllowAmbiguousTypes #-} -- joinA
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImpredicativeTypes #-}
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
import Control.Arrow ((***))

import Data.Function ((&))
import Data.Equality.Graph.Lens
import Data.Equality.Language
import Data.Equality.Graph.Internal (EGraph)
import Data.Equality.Graph.Classes

-- | An e-class analysis with domain @domain@ defined for a language @l@.
--
-- The @domain@ is the type of the domain of the e-class analysis, that is, the
-- type of the data stored in an e-class according to this e-class analysis
class Eq domain => Analysis domain (l :: Type -> Type) where

    -- | When a new e-node is added into a new, singleton e-class, construct a
    -- new value of the domain to be associated with the new e-class, by
    -- accessing the associated data of the node's children
    --
    -- The argument is the e-node term populated with its children data
    --
    -- === Example
    --
    -- @
    -- -- domain = Maybe Double
    -- makeA :: Expr (Maybe Double) -> Maybe Double
    -- makeA = \case
    --     BinOp Div e1 e2 -> liftA2 (/) e1 e2
    --     BinOp Sub e1 e2 -> liftA2 (-) e1 e2
    --     BinOp Mul e1 e2 -> liftA2 (*) e1 e2
    --     BinOp Add e1 e2 -> liftA2 (+) e1 e2
    --     Const x -> Just x
    --     Sym _ -> Nothing
    -- @
    makeA :: l domain -> domain

    -- | When e-classes c1 c2 are being merged into c, join d_c1 and
    -- d_c2 into a new value d_c to be associated with the new
    -- e-class c
    joinA :: domain -> domain -> domain

    -- | Optionally modify the e-class c (based on d_c), typically by adding an
    -- e-node to c. Modify should be idempotent if no other changes occur to
    -- the e-class, i.e., modify(modify(c)) = modify(c)
    --
    -- === Example
    --
    -- Pruning an e-class with a constant value of all its nodes except for the
    -- leaf values, and adding a constant value node
    --
    -- @
    -- modifyA cl eg0
    --   = case eg0^._class cl._data of
    --       Nothing -> eg0
    --       Just d  ->
    --             -- Add constant as e-node
    --         let (new_c,eg1) = represent (Fix (Const d)) eg0
    --             (rep, eg2)  = merge cl new_c eg1
    --             -- Prune all except leaf e-nodes
    --          in eg2 & _class rep._nodes %~ S.filter (F.null .unNode)
    -- @
    modifyA :: ClassId
            -- ^ Id of class @c@ whose new data @d_c@ triggered the modify call
            -> EGraph domain l
            -- ^ E-graph where class @c@ being modified exists
            -> EGraph domain l
            -- ^ E-graph resulting from the modification
    modifyA _ = id
    {-# INLINE modifyA #-}


-- | The simplest analysis that defines the domain to be () and does nothing
-- otherwise
instance forall l. Analysis () l where
  makeA _ = ()
  joinA = (<>)


-- | This instance is not necessarily well behaved for any two analysis, so care
-- must be taken when using it.
--
-- A possible criterion is:
--
-- For any two analysis, where 'modifyA' is called @m1@ and @m2@ respectively,
-- this instance is well behaved if @m1@ and @m2@ commute, and the analysis
-- only change the e-class being modified.
--
-- That is, if @m1@ and @m2@ satisfy the following law:
-- @
-- m1 . m2 = m2 . m1
-- @
--
-- A simple criterion that should suffice for commutativity. If:
--  * The modify function only depends on the analysis value, and
--  * The modify function doesn't change the analysis value
-- Then any two such functions commute.
--
-- Note: there are weaker (or at least different) criteria for this instance to
-- be well behaved.
instance (Language l, Analysis a l, Analysis b l) => Analysis (a, b) l where

  makeA :: l (a, b) -> (a, b)
  makeA g = (makeA @a (fst <$> g), makeA @b (snd <$> g))

  joinA :: (a,b) -> (a,b) -> (a,b)
  joinA (x,y) = joinA @a @l x *** joinA @b @l y

  modifyA :: ClassId -> EGraph (a, b) l -> EGraph (a, b) l
  modifyA c egr =
    let egra = modifyA @a c (egr & _classes._data %~ fst)
        egrb = modifyA @b c (egr & _classes._data %~ snd)
        ca = egra ^._class c
        cb = egrb ^._class c
     in
      egr &
        _class c .~ (EClass c (eClassNodes ca <> eClassNodes cb) (eClassData ca, eClassData cb) (eClassParents ca <> eClassParents cb))
