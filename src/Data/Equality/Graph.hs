{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- {-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-|
   An e-graph efficiently represents a congruence relation over many expressions.

   Based on \"egg: Fast and Extensible Equality Saturation\" https://arxiv.org/abs/2004.03082.
 -}
module Data.Equality.Graph
    (
      -- * Definition of e-graph
      EGraph

      -- ** Transformations
    , represent, add, merge, rebuild
    -- , repair, repairAnal

      -- ** Querying
    , find, canonicalize

      -- * Functions on e-graphs
    , emptyEGraph

      -- ** Low-level operations
    , newEClass, newPointerToClassId

      -- * Re-exports
    , module Data.Equality.Graph.Classes
    , module Data.Equality.Graph.Nodes
    , module Data.Equality.Language
    ) where

import GHC.Exts (Int(..), (+#), (<#), isTrue#)

-- ROMES:TODO: Is the E-Graph a Monad if the analysis data were the type arg? i.e. Monad (EGraph language)?

-- import GHC.Conc
import Prelude hiding (lookup)

import Data.Function
import Data.Bifunctor
import Data.Containers.ListUtils

import Control.Monad
import Control.Monad.Trans.State
import Control.Exception (assert)

import qualified Data.IntMap.Strict as IM
import qualified Data.Set    as S

import Data.Equality.Utils.SizedList

import Data.Equality.Graph.Internal
import Data.Equality.Graph.ReprUnionFind
import qualified Data.Equality.Utils.IntToIntMap as IIM
import Data.Equality.Graph.Classes
import Data.Equality.Graph.Nodes
import Data.Equality.Analysis
import Data.Equality.Language
import Data.Equality.Graph.Lens

import Data.Equality.Utils

-- ROMES:TODO: join things built in paralell?
-- instance Ord1 l => Semigroup (EGraph l) where
--     (<>) eg1 eg2 = undefined -- not so easy
-- instance Ord1 l => Monoid (EGraph l) where
--     mempty = EGraph emptyUF mempty mempty mempty

-- | Represent an expression (in it's fixed point form) in an e-graph.
-- Returns the updated e-graph and the id from the e-class in which it was represented.
represent :: forall a l. (Analysis a l, Language l) => Fix l -> EGraph a l -> (ClassId, EGraph a l)
represent = cata (flip $ \e -> uncurry add . first Node . (`runState` e) . traverse (gets >=> \(x,e') -> x <$ put e'))

-- | Add an e-node to the e-graph
--
-- If the e-node is already represented in this e-graph, the class-id of the
-- class it's already represented in will be returned.
add :: forall a l. (Analysis a l, Language l) => ENode l -> EGraph a l -> (ClassId, EGraph a l)
add uncanon_e egr =
    let !new_en = canonicalize uncanon_e egr

     in case lookupNM new_en (memo egr) of
      Just canon_enode_id -> (find canon_enode_id egr, egr)
      Nothing ->

        let

            -- Make new equivalence class with a new id in the union-find
            (new_eclass_id, new_uf) = makeNewSet (unionFind egr)

            -- New singleton e-class stores the e-node and its analysis data
            -- which is modified according to analysis
            --
            -- The modification also produces a list of expressions to
            -- represent and merge with this class, which we'll do before
            -- returning from this function
            (new_eclass, added_nodes) = modifyA $ EClass new_eclass_id (S.singleton new_en) (makeA @a ((\i -> egr^._class i._data @a) <$> unNode new_en)) mempty

            -- TODO:Performance: All updates can be done to the map first? Parallelize?
            --
            -- Update e-classes by going through all e-node children and adding
            -- to the e-class parents the new e-node and its e-class id
            --
            -- And add new e-class to existing e-classes
            new_parents      = ((new_eclass_id, new_en) |:)
            new_classes      = IM.insert new_eclass_id new_eclass $
                                    foldr  (IM.adjust (_parents %~ new_parents))
                                           (classes egr)
                                           (unNode new_en)

            -- TODO: From egg: Is this needed?
            -- This is required if we want math pruning to work. Unfortunately, it
            -- also makes the invariants tests x4 slower (because they aren't using
            -- analysis) I think there might be another way to ensure math analysis
            -- pruning to work without having this line here.  Comment it out to
            -- check the result on the unit tests.
            -- 
            -- Update: I found a fix for that case: the modifyA function must add
            -- the parents of the pruned class to the worklist for them to be
            -- upward merged. I think it's a good compromise for requiring the user
            -- to do this. Adding the added node to the worklist everytime creates
            -- too much unnecessary work.
            --
            -- Actually I've found more bugs regarding this, and can't fix them
            -- there, so indeed this seems to be necessary for sanity with 'modifyA'
            --
            -- This way we also liberate the user from caring about the worklist
            --
            -- The hash cons invariants test suffer from this greatly but the
            -- saturation tests seem mostly fine?
            --
            -- And adding to the analysis worklist doesn't work, so maybe it's
            -- something else?
            --
            -- So in the end, we do need to addToWorklist to get correct results
            new_worklist     = (new_eclass_id, new_en):worklist egr

            -- Add the e-node's e-class id at the e-node's id
            new_memo         = insertNM new_en new_eclass_id (memo egr)

            -- So we have our almost final e-graph. We just need to represent
            -- and merge in it all expressions which resulted from 'modifyA'
            -- above
            egr1             = egr { unionFind = new_uf
                                   , classes   = new_classes
                                   , worklist  = new_worklist
                                   , memo      = new_memo
                                   }

            egr2             = foldr (representAndMerge new_eclass_id) egr1 added_nodes


         in ( new_eclass_id
            , egr2
            )
{-# INLINABLE add #-}

-- | Merge 2 e-classes by id
merge :: forall a l. (Analysis a l, Language l) => ClassId -> ClassId -> EGraph a l -> (ClassId, EGraph a l)
merge a b egr0 =

  -- Use canonical ids
  let
      a' = find a egr0
      b' = find b egr0
   in
   if a' == b'
     then (a', egr0)
     else
       let
           -- Get classes being merged
           class_a = egr0 ^._class a'
           class_b = egr0 ^._class b'

           -- Leader is the class with more parents
           (leader, leader_class, sub, sub_class) =
               if sizeSL (class_a^._parents) < sizeSL (class_b^._parents)
                  then (b', class_b, a', class_a) -- b is leader
                  else (a', class_a, b', class_b) -- a is leader

           -- Make leader the leader in the union find
           (new_id, new_uf) = unionSets leader sub (unionFind egr0)
                                & first (\n -> assert (leader == n) n)

           -- Update leader class with all e-nodes and parents from the
           -- subsumed class
           (updatedLeader, added_nodes) = leader_class
                                            & _parents %~ (sub_class^._parents <>)
                                            & _nodes   %~ (sub_class^._nodes <>)
                                            & _data    .~ new_data
                                            & modifyA

           new_data = joinA @a @l (leader_class^._data) (sub_class^._data)

           -- Update leader in classes so that it has all nodes and parents
           -- from subsumed class, and delete the subsumed class
           --
           -- Additionally modify the e-class according to the analysis
           new_classes = (IM.insert leader updatedLeader . IM.delete sub) (classes egr0)

           -- Add all subsumed parents to worklist We can do this instead of
           -- adding the new e-class itself to the worklist because it would end
           -- up adding its parents anyway
           new_worklist = toListSL (sub_class^._parents) <> worklist egr0

           -- If the new_data is different from the classes, the parents of the
           -- class whose data is different from the merged must be put on the
           -- analysisWorklist
           new_analysis_worklist =
             (if new_data /= (sub_class^._data)
                then toListSL (sub_class^._parents)
                else mempty) <>
             (if new_data /= (leader_class^._data)
                then toListSL (leader_class^._parents)
                else mempty) <>
             analysisWorklist egr0

           -- ROMES:TODO: The code that makes the -1 * cos test pass when some other things are tweaked
           -- new_memo = foldr (`insertNM` leader) (memo egr0) (sub_class^._nodes)

           -- Build new e-graph
           egr1 = egr0
             { unionFind = new_uf
             , classes   = new_classes
             -- , memo      = new_memo
             , worklist  = new_worklist
             , analysisWorklist = new_analysis_worklist
             }

           egr2 = foldr (representAndMerge leader) egr1 added_nodes

        in (new_id, egr2)
{-# INLINEABLE merge #-}
            

-- | The rebuild operation processes the e-graph's current worklist,
-- restoring the invariants of deduplication and congruence. Rebuilding is
-- similar to other approaches in how it restores congruence; but it uniquely
-- allows the client to choose when to restore invariants in the context of a
-- larger algorithm like equality saturation.
rebuild :: (Analysis a l, Language l) => EGraph a l -> EGraph a l
rebuild (EGraph uf cls mm wl awl) =
  -- empty worklists
  -- repair deduplicated e-classes
  let
    emptiedEgr = EGraph uf cls mm mempty mempty

    wl'   = nubOrd $ bimap (`find` emptiedEgr) (`canonicalize` emptiedEgr) <$> wl
    egr'  = foldr repair emptiedEgr wl'

    awl'  = nubIntOn fst $ first (`find` egr') <$> awl
    egr'' = foldr repairAnal egr' awl'
  in
  -- Loop until worklist is completely empty
  if null (worklist egr'') && null (analysisWorklist egr'')
     then egr''
     else rebuild egr'' -- ROMES:TODO: Doesn't seem to be needed at all in the testsuite.
{-# INLINEABLE rebuild #-}

-- ROMES:TODO: find repair_id could be shared between repair and repairAnal?

-- | Repair a single worklist entry.
repair :: forall a l. (Analysis a l, Language l) => (ClassId, ENode l) -> EGraph a l -> EGraph a l
repair (repair_id, node) egr =

   -- TODO We're no longer deleting the uncanonicalized node, how much does it matter that the structure keeps growing?

   case insertLookupNM node repair_id (memo egr) of

      (Nothing, memo') -> egr { memo = memo' } -- new memo with inserted node

      (Just existing_class, memo') -> snd (merge existing_class repair_id egr{memo = memo'})
{-# INLINE repair #-}

-- | Repair a single analysis-worklist entry.
repairAnal :: forall a l. (Analysis a l, Language l) => (ClassId, ENode l) -> EGraph a l -> EGraph a l
repairAnal (repair_id, node) egr =
    let
        c1                = egr^._class repair_id
        new_data          = joinA @a @l (c1^._data) (makeA @a ((\i -> egr^._class i^._data @a) <$> unNode node))
        (c2, added_nodes) = modifyA (c1 & _data .~ new_data)
    in
    -- Take action if the new_data is different from the existing data
    if c1^._data /= new_data
        -- Merge result is different from original class data, update class
        -- with new_data
       then
        let
            new_classes = IM.insert repair_id c2 (classes egr)
            egr1 = egr { analysisWorklist = toListSL (c1^._parents) <> analysisWorklist egr
                       , classes = new_classes
                       }
            egr2 = foldr (representAndMerge repair_id) egr1 added_nodes
         in egr2
       else egr
{-# INLINE repairAnal #-}

-- | Canonicalize an e-node
--
-- Two e-nodes are equal when their canonical form is equal. Canonicalization
-- makes the list of e-class ids the e-node holds a list of canonical ids.
-- Meaning two seemingly different e-nodes might be equal when we figure out
-- that their e-class ids are represented by the same e-class canonical ids
--
-- canonicalize(𝑓(𝑎,𝑏,𝑐,...)) = 𝑓((find 𝑎), (find 𝑏), (find 𝑐),...)
canonicalize :: Functor l => ENode l -> EGraph a l -> ENode l
canonicalize (Node enode) eg = Node $ fmap (`find` eg) enode
{-# INLINE canonicalize #-}

-- | Find the canonical representation of an e-class id in the e-graph
--
-- Invariant: The e-class id always exists.
find :: ClassId -> EGraph a l -> ClassId
find cid = findRepr cid . unionFind
{-# INLINE find #-}

-- | The empty e-graph. Nothing is represented in it yet.
emptyEGraph :: Language l => EGraph a l
emptyEGraph = EGraph emptyUF mempty mempty mempty mempty
{-# INLINE emptyEGraph #-}

-- | Creates an empty e-class in an e-graph, with the explicitly given domain analysis data.
-- (That is, an e-class with no e-nodes)
newEClass :: (Analysis a l, Language l) => a -> EGraph a l -> (ClassId, EGraph a l)
newEClass adata egr =
  let
    -- Make new equivalence class with a new id in the union-find
    (new_eclass_id, new_uf) = makeNewSet (unionFind egr)

    -- New empty e-class stores just the analysis data
    new_eclass = EClass new_eclass_id S.empty adata mempty
   in ( new_eclass_id
      , egr { unionFind = new_uf
            , classes   = IM.insert new_eclass_id new_eclass (classes egr)
            }
      )
{-# INLINEABLE newEClass #-}

-- | Create a mapping from some class-id that does not exist in the e-graph to
-- the given e-class id target. In practice, this basically creates an
-- alias from the a given class-id to the e-class id of the target
--
-- If, instead, you want to create a mapping from an existing class-id to another one, use 'merge'.
--
-- Under the hood, this operation will bump the union find counter for next-ids
-- to the given id+1 and add an entry to the union find from given id to the
-- given target id.
--
-- This means that all e-class ids up to the new pointer id will be considered to be in use.
--
-- INVARIANT: The given e-class pointer does not exist in the e-graph
newPointerToClassId :: ClassId -- ^ Given Id (pointer) that will point to the target
                    -> ClassId -- ^ The target id
                    -> EGraph a l -> EGraph a l
newPointerToClassId (I# pointer) (I# target) egr =
   egr { unionFind = case unionFind egr of
                       RUF im _size ->
                         if isTrue# (pointer <# _size)
                            then error $ "newPointerToClassId: given pointer id (" ++ show (I# pointer) ++ ") already exists in the e-graph"
                            else RUF (IIM.insert pointer target im) (pointer +# 1#)
       }
{-# INLINEABLE newPointerToClassId #-}

-- | Represent an expression (in fix-point form) and merge it with the e-class with the given id
representAndMerge :: (Analysis a l, Language l) => ClassId -> Fix l -> EGraph a l -> EGraph a l
representAndMerge o f g = case represent f g of
                        (i, e) -> snd $ merge o i e
