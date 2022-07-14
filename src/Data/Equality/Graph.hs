{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Equality.Graph
    ( module Data.Equality.Graph
    , module Data.Equality.Graph.Classes
    , module Data.Equality.Graph.Nodes
    ) where

import Data.Bifunctor

import Data.Fix

import Data.Functor.Classes

import Control.Monad
import Control.Monad.State

import qualified Data.Map.Strict     as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

import Data.Equality.Graph.ReprUnionFind
import Data.Equality.Graph.Classes
import Data.Equality.Graph.Nodes

-- | E-graph stateful computation
type EGS s = State (EGraph s)

runEGS :: EGraph s -> EGS s a -> (a, EGraph s)
runEGS = flip runState

egraph :: Language l => EGS l a -> EGraph l
egraph = snd . runEGS emptyEGraph

-- | E-graph
--
-- @s@ for the e-node term
-- @nid@ type of e-node ids
data EGraph l = EGraph
    { unionFind :: ReprUnionFind -- ^ Union find like structure to find canonical representation of an e-class id
    , classes   :: ClassIdMap (EClass l) -- ^ Map canonical e-class ids to their e-classes
    , memo      :: M.Map (ENode l) ClassId -- ^ Hashcons maps all canonical e-nodes to their e-class ids
    , worklist  :: [ClassId] -- ^ e-class ids that need to be upward merged
    }

type Memo l = M.Map (ENode l) ClassId

-- ROMES:TODO: Monoid instance to join things built in paralell?

instance Show1 l => Show (EGraph l) where
    show (EGraph a b c e) =
        "UnionFind: " <> show a <>
            "\n\nE-Classes: " <> show b <>
                "\n\nHashcons: " <> show c <>
                    "\n\nWorklist: " <> show e

-- | Represent an expression (@Fix lang@) in an e-graph
represent :: Language lang => Fix lang -> EGS lang ClassId
-- Represent each sub-expression and add the resulting e-node to the e-graph
represent (Fix l) = traverse represent l >>= add . Node

-- | Add an e-node to the e-graph
--
-- E-node lookup depends on e-node correctly defining equality
add :: forall l. Language l => ENode l -> EGS l ClassId
add uncanon_e = do
    eg@EGraph { memo = encls } <- get
    let new_en = canonicalize uncanon_e eg
    case M.lookup new_en encls of
      Just canon_enode_id -> return canon_enode_id
      Nothing -> do
        -- Add new singleton e-class with the e-node
        new_eclass_id <- singletonClass new_en
        -- Update e-classes by going through all e-node children and adding
        -- to the e-class parents the new e-node and its e-class id
        forM_ (children new_en) $ \eclass_id -> do
            -- Update canonical e-class of child e-class
            -- ROMES:TODO: does the find operation need to take into consideration the new e_class?
            modifyClasses (IM.update (\e_class -> Just $ e_class { eClassParents = (new_en, new_eclass_id):eClassParents e_class }) (find eclass_id eg))
        -- Add the e-node's e-class id at the e-node's id
        modifyMemo (M.insert new_en new_eclass_id)
        return new_eclass_id


-- | Merge 2 e-classes by id
merge :: forall l. Language l => ClassId -> ClassId -> EGS l ClassId
merge a b = do
    eg <- get
    -- Use canonical ids
    let a' = find a eg
        b' = find b eg
    if a' == b'
       then return a'
       else do
           let class_a@(EClass _ _ pa) = snd $ getClass a' eg
               class_b@(EClass _ _ pb) = snd $ getClass b' eg

           -- Leader is the class with more parents
           let (leader, sub, sub_class) =
                   if length pa < length pb
                      then (b', a', class_a) -- b is leader
                      else (a', b', class_b) -- a is leader

           new_id <- mergeUnionFindClasses leader sub

           eg' <- get

           -- Delete subsumed? class
           modifyClasses (IM.delete sub)

           -- Update leader class with all e-nodes and parents from the
           -- subsumed class
           let updateLeader (EClass i ns ps) = Just $ EClass i new_nodes new_parents where
                   new_nodes = eClassNodes sub_class <> ns
                   -- ROMES:TODO I must @map (second (`find` eg)) here to
                   -- correct the result, but the original implementation
                   -- doesn't do it quite here, if I saw correctly
                   new_parents = map (second (`find` eg')) $ eClassParents sub_class <> ps
           modifyClasses (IM.update updateLeader leader)

           -- Recanonize all leader nodes in memo
           --
           -- ROMES:TODO Rebuild  should maintain both invariants instead of
           -- merge...
           eg'' <- get
           let EClass _ ns _ = snd $ getClass leader eg'' :: EClass l
           forM_ (S.toList ns) $ \l -> do
               -- Remove stale term
               modifyMemo (M.delete l)
               -- Insert canonicalized term
               modifyMemo (M.insert (l `canonicalize` eg) leader)


           addToWorklist new_id
           return new_id
            

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

-- | Find the canonical representation of an e-class id in the e-graph
-- Invariant: The e-class id always exists.
find :: ClassId -> EGraph l -> ClassId
find cid = unsafeUnpack . findRepr cid . unionFind
    where
        unsafeUnpack Nothing  = error $ "The impossible happened: Couldn't find representation of e-node " <> show cid
        unsafeUnpack (Just x) = x

rebuild :: Language l => EGS l ()
rebuild = do
    eg <- get

    -- empty the worklist into a local variable
    wl <- clearWorkList

    -- canonicalize and deduplicate the class refs to save calls to repair
    let todo = S.fromList $ map (`find` eg) wl

    -- repair deduplicated eclasses
    forM_ todo repair

    -- Loop until worklist is completely empty
    wl' <- gets worklist
    unless (null wl') rebuild


repair :: Language l => ClassId -> EGS l ()
repair repair_id = do
    (_, EClass _ _ parents) <- gets (getClass repair_id)

    -- Update the hashcons so it always points
    -- canonical enodes to canonical eclasses
    forM_ parents $ \(node, eclass_id) -> do
        modifyMemo (M.delete node)
        eg <- get
        modifyMemo (M.insert (canonicalize node eg) (find eclass_id eg))

    new_parents <- M.toList <$> go parents mempty
    modifyClasses (IM.update (\eclass -> Just $ eclass { eClassParents = new_parents }) repair_id)
        where
            go :: Language l => [(ENode l, ClassId)] -> Memo l -> EGS l (Memo l)
            go [] s = return s
            go ((node, eclass_id):xs) s = do
                -- Deduplicate the parents, noting that equal parents get merged and put on
                -- the worklist 
                node' <- gets (canonicalize node)
                case M.lookup node' s of
                  Nothing -> return ()
                  Just ci -> void $ merge eclass_id ci
                eg <- get
                go xs (M.insert node' (find eclass_id eg) s)

-- | Add a singleton e-class with the e-node (first arg) to the e-graph
singletonClass :: ENode s -> EGS s ClassId
singletonClass en = do
    -- Make new equivalence class with a new id in the union-find
    new_id <- createUnionFindClass
    -- New singleton e-class stores the e-node id of the e-node
    let new_eclass = EClass new_id (S.singleton en) []
    -- Add new e-class to existing e-classes
    modifyClasses (IM.insert new_id new_eclass)
    -- Return created e-class id
    return new_id

addToWorklist :: ClassId -> EGS s ()
addToWorklist i =
    modify (\egr -> egr { worklist = i:worklist egr})

-- | Clear the e-graph worklist and return the existing work items
clearWorkList :: EGS s [ClassId]
clearWorkList = do
    wl <- gets worklist
    modify (\egr -> egr { worklist = [] })
    return wl

-- | Get an e-class from an e-graph given its e-class id
--
-- Returns the canonical id of the class and the class itself
--
-- We'll find its canonical representation and then get it from the e-classes map
--
-- Invariant: The e-class always exists.
getClass :: ClassId -> EGraph s -> (ClassId, EClass s)
getClass cid egr=
    let canon_id = find cid egr
     in (canon_id, classes egr IM.! canon_id)

-- | Extend the existing UnionFind equivalence classes with a new one and
-- return the new id
createUnionFindClass :: EGS s ClassId
createUnionFindClass = do
    uf <- gets unionFind
    let (new_id, new_uf) = makeNewSet uf
    modify (\egr -> egr { unionFind = new_uf })
    return new_id

-- | Merge two equivalent classes in the union find
mergeUnionFindClasses :: ClassId -> ClassId -> EGS s ClassId
mergeUnionFindClasses a b = do
    uf <- gets unionFind
    let (new_id, new_uf) = unionSets a b uf
    modify (\egr -> egr { unionFind = new_uf })
    return new_id


modifyClasses :: (ClassIdMap (EClass s) -> ClassIdMap (EClass s)) -> EGS s ()
modifyClasses f = modify (\egr -> egr { classes = f (classes egr) })

modifyMemo :: (Memo l -> Memo l) -> EGS l ()
modifyMemo f = modify (\egr -> egr { memo = f (memo egr) })

emptyEGraph :: Language l => EGraph l
emptyEGraph = EGraph emptyUF mempty mempty []

getSize :: EGS l Int
getSize = gets sizeEGraph

sizeEGraph :: EGraph l -> Int
sizeEGraph (EGraph { unionFind = (RUF im) }) = IM.size im

