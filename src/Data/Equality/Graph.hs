{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-} -- tmp show
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Equality.Graph
    ( module Data.Equality.Graph
    , module Data.Equality.Graph.Classes
    , module Data.Equality.Graph.Nodes
    , module Data.Equality.Language
    ) where

-- import GHC.Conc

import Data.Function

import Data.Functor.Classes

import qualified Data.IntMap.Strict as IM
import qualified Data.Set    as S

import Data.Equality.Graph.ReprUnionFind
import Data.Equality.Graph.Classes
import Data.Equality.Graph.Nodes
import Data.Equality.Analysis
import Data.Equality.Language
import Data.Equality.Graph.Lens

-- | E-graph
--
-- @s@ for the e-node term
-- @nid@ type of e-node ids
data EGraph l = EGraph
    { unionFind :: !ReprUnionFind           -- ^ Union find like structure to find canonical representation of an e-class id
    , classes   :: !(ClassIdMap (EClass l)) -- ^ Map canonical e-class ids to their e-classes
    , memo      :: !(Memo l)                -- ^ Hashcons maps all canonical e-nodes to their e-class ids
    , worklist  :: !(Worklist l)               -- ^ e-class ids that needs repair and the class it's in
    , analysisWorklist :: !(Worklist l)        -- ^ like 'worklist' but for analysis repairing
    }

type Memo l = NodeMap l ClassId
type Worklist l = NodeMap l ClassId

-- ROMES:TODO: join things built in paralell?
-- instance Ord1 l => Semigroup (EGraph l) where
--     (<>) eg1 eg2 = undefined -- not so easy
-- instance Ord1 l => Monoid (EGraph l) where
--     mempty = EGraph emptyUF mempty mempty mempty

instance (Show (Domain l), Show1 l) => Show (EGraph l) where
    show (EGraph a b c d e) =
        "UnionFind: " <> show a <>
            "\n\nE-Classes: " <> show b <>
                "\n\nHashcons: " <> show c <>
                    "\n\nWorklist: " <> show d <>
                        "\n\nAnalWorklist: " <> show e


-- | Add an e-node to the e-graph
--
-- E-node lookup depends on e-node correctly defining equality
add :: forall l. Language l => ENode l -> EGraph l -> (ClassId, EGraph l)
add uncanon_e egr =
    let !new_en = {-# SCC "-2" #-} canonicalize uncanon_e egr

     in case {-# SCC "-1" #-} lookupNM new_en (memo egr) of
      Just canon_enode_id -> {-# SCC "0" #-} (find canon_enode_id egr, egr)
      Nothing ->

        let

            -- Make new equivalence class with a new id in the union-find
            (new_eclass_id, new_uf) = makeNewSet (unionFind egr)

            -- New singleton e-class stores the e-node and its analysis data
            new_eclass       = EClass new_eclass_id (S.singleton new_en) (makeA new_en egr) mempty

            -- TODO:Performance: All updates can be done to the map first? Parallelize?
            --
            -- Update e-classes by going through all e-node children and adding
            -- to the e-class parents the new e-node and its e-class id
            --
            -- And add new e-class to existing e-classes
            new_parents      = insertNM new_en new_eclass_id
            new_classes      = {-# SCC "2" #-} IM.insert new_eclass_id new_eclass $
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
            new_worklist     = {-# SCC "4" #-} insertNM new_en new_eclass_id (worklist egr)

            -- Add the e-node's e-class id at the e-node's id
            new_memo         = {-# SCC "5" #-} insertNM new_en new_eclass_id (memo egr)

         in ( new_eclass_id

            , egr { unionFind = new_uf
                  , classes   = new_classes
                  , worklist  = new_worklist
                  , memo     = new_memo
                  }

                  -- Modify created node according to analysis
                  & {-# SCC "6" #-} modifyA new_eclass_id

            )
{-# SCC add #-}

-- | Merge 2 e-classes by id
merge :: forall l. Language l => ClassId -> ClassId -> EGraph l -> (ClassId, EGraph l)
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
               if (sizeNM (class_a^._parents)) < (sizeNM (class_b^._parents))
                  then (b', class_b, a', class_a) -- b is leader
                  else (a', class_a, b', class_b) -- a is leader

           -- Make leader the leader in the union find
           (new_id, new_uf) = unionSets leader sub (unionFind egr0)

           -- Update leader class with all e-nodes and parents from the
           -- subsumed class
           updatedLeader = leader_class & _parents %~ (<> sub_class^._parents)
                                        & _nodes   %~ (<> sub_class^._nodes)
                                        & _data    .~ new_data
           new_data = joinA @l (leader_class^._data) (sub_class^._data)

           -- Update leader in classes so that it has all nodes and parents
           -- from subsumed class, and delete the subsumed class
           new_classes = ((IM.insert leader updatedLeader) . (IM.delete sub)) (classes egr0)

           -- Add all subsumed parents to worklist We can do this instead of
           -- adding the new e-class itself to the worklist because it would end
           -- up adding its parents anyway
           new_worklist = sub_class^._parents <> (worklist egr0)

           -- If the new_data is different from the classes, the parents of the
           -- class whose data is different from the merged must be put on the
           -- analysisWorklist
           new_analysis_worklist =
             (if new_data /= (leader_class^._data)
                then leader_class^._parents
                else mempty) <>
             (if new_data /= (sub_class^._data)
                 then sub_class^._parents
                 else mempty) <>
             (analysisWorklist egr0)

           -- ROMES:TODO: The code that makes the -1 * cos test pass when some other things are tweaked
           -- new_memo = foldr (`insertNM` leader) (memo egr0) (sub_class^._nodes)

           -- Build new e-graph
           new_egr = egr0
             { unionFind = new_uf
             , classes   = new_classes
             -- , memo      = new_memo
             , worklist  = new_worklist
             , analysisWorklist = new_analysis_worklist
             }

             -- Modify according to analysis
             & modifyA new_id

        in (new_id, new_egr)
{-# SCC merge #-}
            
rebuild :: Language l => EGraph l -> EGraph l
rebuild (EGraph uf cls mm wl awl) =
  -- empty worklists
  -- repair deduplicated e-classes
  let
    egr'  = foldrWithKeyNM' repair (EGraph uf cls mm mempty mempty) wl
    egr'' = foldrWithKeyNM' repairAnal egr' awl
  in
  -- Loop until worklist is completely empty
  if null (worklist egr'') && null (analysisWorklist egr'')
     then egr''
     else rebuild egr''

{-# SCC rebuild #-}

repair :: forall l. Language l => ENode l -> ClassId -> EGraph l -> EGraph l
repair node repair_id egr =

   case insertLookupNM (node `canonicalize` egr) (find repair_id egr) (deleteNM node $ memo egr) of-- TODO: I seem to really need it. Is find needed? (they don't use it)

      (Nothing, memo2) -> egr { memo = memo2 } -- Return new memo but delete uncanonicalized node

      (Just existing_class, memo2) -> snd (merge existing_class repair_id egr{memo = memo2})
{-# SCC repair #-}

repairAnal :: forall l. Language l => ENode l -> ClassId -> EGraph l -> EGraph l
repairAnal node repair_id egr =
    let
        canon_id = find repair_id egr
        c        = egr^._class canon_id
        new_data = joinA @l (c^._data) (makeA node egr)
    in
    -- Take action if the new_data is different from the existing data
    if c^._data /= new_data
        -- Merge result is different from original class data, update class
        -- with new_data
       then egr { analysisWorklist = c^._parents <> analysisWorklist egr
                }
                & _class canon_id._data .~ new_data
                & modifyA canon_id
       else egr
{-# SCC repairAnal #-}

-- | Canonicalize an E-Node
--
-- Two e-nodes are equal when their canonical form is equal. Canonicalization
-- makes the list of e-class ids the e-node holds a list of canonical ids.
-- Meaning two seemingly different e-nodes might be equal when we figure out
-- that their e-class ids are represented by the same e-class canonical ids
--
-- canonicalize(f(a,b,c,...)) = f((find a), (find b), (find c),...)
canonicalize :: Functor l => ENode l -> EGraph l -> ENode l
canonicalize (Node enode) eg = Node $ fmap (`find` eg) enode
{-# SCC canonicalize #-}

-- | Find the canonical representation of an e-class id in the e-graph
-- Invariant: The e-class id always exists.
find :: ClassId -> EGraph l -> ClassId
find cid = findRepr cid . unionFind
{-# INLINE find #-}

emptyEGraph :: Language l => EGraph l
emptyEGraph = EGraph emptyUF mempty mempty mempty mempty
{-# INLINE emptyEGraph #-}
