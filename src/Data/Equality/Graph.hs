{-# LANGUAGE TypeApplications #-}
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
    , modify, get, gets
    ) where

-- import GHC.Conc

import Data.Function
import Data.Foldable (foldl')

import Data.Functor.Classes

import Control.Monad
import Control.Monad.Trans.State.Strict

import qualified Data.IntMap.Strict as IM
import qualified Data.Set    as S

import Data.Equality.Utils
import Data.Equality.Graph.ReprUnionFind
import Data.Equality.Graph.Classes
import Data.Equality.Graph.Nodes
import Data.Equality.Language
import Data.Equality.Graph.Lens

-- | E-graph stateful computation
type EGS s = State (EGraph s)

runEGS :: EGraph s -> EGS s a -> (a, EGraph s)
runEGS = flip runState
{-# INLINE runEGS #-}

egraph :: Language l => EGS l a -> EGraph l
egraph = snd . runEGS emptyEGraph
{-# INLINE egraph #-}

-- | E-graph
--
-- @s@ for the e-node term
-- @nid@ type of e-node ids
data EGraph l = EGraph
    { unionFind :: !ReprUnionFind           -- ^ Union find like structure to find canonical representation of an e-class id
    , classes   :: ClassIdMap (EClass l) -- ^ Map canonical e-class ids to their e-classes
    , memo      :: Memo l                -- ^ Hashcons maps all canonical e-nodes to their e-class ids
    , worklist  :: Worklist l               -- ^ e-class ids that needs repair and the class it's in
    , analysisWorklist :: Worklist l        -- ^ like 'worklist' but for analysis repairing
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

-- | Represent an expression (@Fix lang@) in an e-graph
represent :: Language lang => Fix lang -> EGS lang ClassId
-- Represent each sub-expression and add the resulting e-node to the e-graph
represent = cata $ sequence >=> add . Node
{-# SCC represent #-}

-- | Add an e-node to the e-graph
--
-- E-node lookup depends on e-node correctly defining equality
add' :: forall l. Language l => ENode l -> EGraph l -> (ClassId, EGraph l)
add' uncanon_e egr =
    let new_en = {-# SCC "-2" #-} canonicalize uncanon_e egr

     in case {-# SCC "-1" #-} lookupNM new_en (memo egr) of
      Just canon_enode_id -> (canon_enode_id, egr)
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
            new_classes      = {-# SCC "2" #-} IM.insert new_eclass_id new_eclass $
                                  foldl' (flip $ IM.adjust (_parents %~ insertNM new_en new_eclass_id))
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
{-# SCC add' #-}

-- | Add an e-node to the e-graph
--
-- E-node lookup depends on e-node correctly defining equality
add :: forall l. Language l => ENode l -> EGS l ClassId
add = StateT . fmap pure . add'
{-# INLINE add #-}


-- | Merge 2 e-classes by id
merge :: forall l. Language l => ClassId -> ClassId -> EGS l ClassId
merge a b = do

    -- Use canonical ids
    a' <- findMut a
    b' <- findMut b

    if a' == b'
       then return a'
       else get >>= \egr0 -> do

           -- Get classes being merged
           let
               class_a = egr0 ^._class a'
               class_b = egr0 ^._class b'

           -- Leader is the class with more parents
           let (leader, leader_class, sub, sub_class) =
                   if ({-# SCC "l1" #-} sizeNM (class_a^._parents)) < ({-# SCC "l2" #-} sizeNM (class_b^._parents))
                      then (b', class_b, a', class_a) -- b is leader
                      else (a', class_a, b', class_b) -- a is leader

           new_id <- mergeUnionFindClasses leader sub

           -- Delete subsumed class
           modify (_classes %~ IM.delete sub)

           -- Add all subsumed parents to worklist
           -- We can do this instead of adding the new e-class itself to the worklist because it'll end up in doing this anyway
           addToWorklist (sub_class^._parents)

           -- Update leader class with all e-nodes and parents from the
           -- subsumed class
           let updatedLeader = leader_class & _parents %~ (<> sub_class^._parents)
                                            & _nodes   %~ (<> sub_class^._nodes)
                                            & _data    .~ new_data
               new_data = joinA @l (leader_class^._data) (sub_class^._data)

           -- If the new_data is different from the classes, the parents of the
           -- class whose data is different from the merged must be put on the
           -- analysisWorklist
           when (new_data /= (leader_class^._data)) $
               addToAnalysisWorklist (leader_class^._parents)
           when (new_data /= (sub_class^._data)) $
               addToAnalysisWorklist (leader_class^._parents)

           -- Update leader so that it has all nodes and parents from
           -- subsumed class
           modify (_class leader .~ updatedLeader)

           modify (modifyA new_id)

           return new_id
{-# SCC merge #-}
            

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

canonicalizeMut :: Traversable l => ENode l -> EGS l (ENode l)
canonicalizeMut (Node enode) = Node <$> traverse findMut enode
{-# SCC canonicalizeMut #-}

-- | Find the canonical representation of an e-class id in the e-graph
-- Invariant: The e-class id always exists.
-- With path compression
findMut :: ClassId -> EGS l ClassId
findMut cid = do
  (i, new_uf) <- gets (findReprMut cid . unionFind)
  modify (\egr -> egr { unionFind = new_uf })
  return i
    -- where
    --     unsafeUnpack Nothing  = error $ "The impossible happened: Couldn't find representation of e-node " <> show cid
    --     unsafeUnpack (Just x) = x
{-# INLINE findMut #-}

-- | Find the canonical representation of an e-class id in the e-graph
-- Invariant: The e-class id always exists.
find :: ClassId -> EGraph l -> ClassId
find cid = findRepr cid . unionFind
{-# INLINE find #-}

rebuild :: Language l => EGS l ()
rebuild = do
    -- empty the worklist into a local variable
    wl  <- clearWorkList
    awl <- clearAnalysisWorkList

    -- repair deduplicated eclasses
    traverseWithKeyNM repair wl

    traverseWithKeyNM repairAnal awl

    -- Loop until worklist is completely empty
    wl'  <- gets worklist
    awl' <- gets analysisWorklist
    unless (null wl' && null awl') rebuild
{-# SCC rebuild #-}

repair' :: forall l. Language l => ENode l -> ClassId -> EGraph l -> EGraph l
repair' node repair_id egr =

  let (canon_id, new_uf) = findReprMut repair_id (unionFind egr)
      (canon_node, new_egr) = runState (canonicalizeMut node) egr

   in case insertLookupNM canon_node canon_id (deleteNM node $ memo egr) of-- TODO: I seem to really need it. Is find needed? (they don't use it)

      (Nothing, memo2) -> new_egr { memo = memo2, unionFind = new_uf } -- Return new memo but delete uncanonicalized node

      (Just existing_class, memo2) -> snd $ runState (merge existing_class repair_id) new_egr{ memo = memo2, unionFind = new_uf }

{-# SCC repair' #-}

repair :: forall l. Language l => ENode l -> ClassId -> EGS l ()
repair x y = StateT (return . ((),) . repair' x y)
{-# INLINE repair #-}

repairAnal :: forall l. Language l => ENode l -> ClassId -> EGS l ()
repairAnal node repair_id = do

    canon_id <- findMut repair_id
    egr <- get

    let
        c        = egr^._class canon_id
        new_data = joinA @l (c^._data) (makeA node egr)

    -- Take action if the new_data is different from the existing data
    when (c^._data /= new_data) $ do
        -- Merge result is different from original class data, update class
        -- with new_data
        put (egr & _class canon_id._data .~ new_data)
        addToAnalysisWorklist (c^._parents)
        modify (modifyA canon_id)
{-# SCC repairAnal #-}


addToWorklist :: Ord1 l => Worklist l -> EGS l ()
addToWorklist li =
    modify (\egr -> egr { worklist = li <> worklist egr})
{-# SCC addToWorklist #-}

-- | Clear the e-graph worklist and return the existing work items
clearWorkList :: Ord1 l => EGS l (Worklist l)
clearWorkList = do
    wl <- gets worklist
    modify (\egr -> egr { worklist = mempty })
    return wl
{-# SCC clearWorkList #-}

addToAnalysisWorklist :: Ord1 l => Worklist l -> EGS l ()
addToAnalysisWorklist lx =
    modify (\egr -> egr { analysisWorklist = lx <> analysisWorklist egr})
{-# SCC addToAnalysisWorklist #-}

clearAnalysisWorkList :: Ord1 l => EGS l (Worklist l)
clearAnalysisWorkList = do
    wl <- gets analysisWorklist
    modify (\egr -> egr { analysisWorklist = mempty })
    return wl
{-# SCC clearAnalysisWorkList #-}

-- | Merge two equivalent classes in the union find
mergeUnionFindClasses :: ClassId -> ClassId -> EGS s ClassId
mergeUnionFindClasses a b = do
    uf <- gets unionFind
    let (new_id, new_uf) = unionSets a b uf
    modify (\egr -> egr { unionFind = new_uf })
    return new_id
{-# SCC mergeUnionFindClasses #-}

emptyEGraph :: Language l => EGraph l
emptyEGraph = EGraph emptyUF mempty mempty mempty mempty
{-# INLINE emptyEGraph #-}
