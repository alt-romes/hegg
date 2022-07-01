{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module EGraph
    ( ClassId, ENode(..)
    , module EGraph
    ) where

import Debug.Trace
import Data.Bifunctor

import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

import EGraph.ReprUnionFind
import EGraph.EClass
import EGraph.ENode

-- | E-graph stateful computation
type EGS s = State (EGraph s)

runEGS :: EGraph s -> EGS s a -> (a, EGraph s)
runEGS = flip runState

-- | E-graph
--
-- @s@ for the e-node term
-- @nid@ type of e-node ids
data EGraph s = EGraph
    { eUnionFind    :: ReprUnionFind -- ^ Union find like structure to find canonical representation of an e-class id
    , eClasses      :: ClassIdMap (EClass s) -- ^ Map (canonical?) e-class ids to their e-classes
    , eNodesClasses :: Map s ClassId -- ^ Hashcons maps ids of all canonical e-nodes to their e-class ids
    , eNodes        :: Map s (ENode s) -- ^ Map e-node ids to e-nodes
    , worklist      :: [ClassId] -- ^ e-class ids that need to be upward merged
    }

instance Show s => Show (EGraph s) where
    show (EGraph a b c d e) =
        "UnionFind: " <> show a <>
            "\n\nE-Classes: " <> show b <>
                "\n\nHashcons: " <> show c <>
                    "\n\nNodes: " <> show d <>
                        "\n\nWorklist: " <> show e

-- | Add an e-node to the e-graph
add :: (Ord s, Show s) => ENode s -> EGS s ClassId
add uncanon_e = do
    egraph@(EGraph { eNodesClasses = encls }) <- get
    let new_en@(ENode enode_id children) = canonicalize uncanon_e egraph
    -- ROMES:TODO Lookup is wrong?, not considering canonical children
    case M.lookup enode_id encls of
      Just canon_enode_id -> return canon_enode_id
      Nothing -> trace ("didn't find " <> show enode_id <> " in " <> show encls) $ do
        -- Add new singleton e-class with the e-node
        new_eclass_id <- singletonClass new_en
        -- Update e-classes by going through all e-node children and adding
        -- to the e-class parents the new e-node and its e-class id
        forM_ children $ \eclass_id -> do
            -- Update canonical e-class of child e-class
            -- ROMES:TODO: does the find operation need to take into consideration the new e_class?
            modifyClasses (IM.update (\e_class -> Just $ e_class { eClassParents = (enode_id, new_eclass_id):(eClassParents e_class) }) (find eclass_id egraph))
        -- Add the e-node's e-class id at the e-node's id
        modifyNodesClasses (M.insert enode_id new_eclass_id)
        return new_eclass_id


-- | Merge 2 e-classes by id
merge :: ClassId -> ClassId -> EGS s ClassId
merge a b = do
    eg <- get
    if find a eg == find b eg
       then return (find a eg)
       else do
           new_id <- mergeUnionFindClasses a b
           -- ROMES:TODO: Remove b from eClasses?
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
canonicalize :: ENode s -> EGraph s -> ENode s
canonicalize (ENode x ls) egraph = ENode x (map (flip find egraph) ls)

-- | Find the canonical representation of an e-class id in the e-graph
-- Invariant: The e-class id always exists.
find :: ClassId -> EGraph s -> ClassId
find cid = unsafeUnpack . findRepr cid . eUnionFind
    where
        unsafeUnpack Nothing  = error $ "The impossible happened: Couldn't find representation of e-node " <> show cid
        unsafeUnpack (Just x) = x

rebuild :: Ord s => EGS s ()
rebuild = do
    eg <- get
    wl <- clearWorkList
    let todo = S.fromList $ map (flip find eg) wl
    forM_ todo repair

repair :: Ord s => ClassId -> EGS s ()
repair repair_id = do
    (j, EClass ei nodes parents) <- getClass repair_id <$> get
    -- Sanity check
    unless (repair_id == j) (error "repair should only be called on canonical ids")
    -- Update the hashcons so it always points
    -- canonical enodes to canonical eclasses
    forM_ parents $ \(node_id, eclass_id) -> do
        modifyNodesClasses (M.delete node_id)
        -- Get canonicalized node from id|->node map
        node <- getNode node_id <$> get
        node <- canonicalize node <$> get
        -- Put canonicalized node in node map
        modifyNodes (M.insert node_id node)
        eg <- get
        modifyNodesClasses (M.insert node_id (find eclass_id eg))

    new_parents <- fmap (first eNodeId) . M.toList <$> go parents M.empty
    modifyClasses (IM.update (\eclass -> Just $ eclass { eClassParents = new_parents }) repair_id)
        where
            go :: Ord s => [(s, ClassId)] -> Map (ENode s) ClassId -> EGS s (Map (ENode s) ClassId)
            go [] s = return s
            go ((node_id, eclass_id):xs) s = do
                -- Deduplicate the parents, noting that equal parents get merged and put on
                -- the worklist 
                node <- getNode node_id <$> get
                node <- canonicalize node <$> get
                case M.lookup node s of
                  Nothing -> return ()
                  Just ci -> void $ merge eclass_id ci
                eg <- get
                go xs (M.insert node (find eclass_id eg) s)

class ERepr a r where
    -- | Represent an expression with an e-graph computation that returns the
    -- e-class id that represents the expression
    represent :: a -> EGS r ClassId
    extract   :: ENode r -> EGraph r -> a

-- | Add a singleton e-class with the e-node (first arg) to the e-graph
singletonClass :: Ord s => ENode s -> EGS s ClassId
singletonClass en = do
    -- Make new equivalence class with a new id in the union-find
    new_id <- createUnionFindClass
    -- New singleton e-class stores the e-node id of the e-node
    let new_eclass = EClass new_id (S.singleton (eNodeId en) ) []
    -- Add new e-class to existing e-classes
    modifyClasses (IM.insert new_id new_eclass)
    -- New e-node is added at its id to the e-nodes map
    modifyNodes   (M.insert (eNodeId en) en)
    -- Return created e-class id
    return new_id

addToWorklist :: ClassId -> EGS s ()
addToWorklist i =
    modify (\egraph -> egraph { worklist = i:worklist egraph })

-- | Clear the e-graph worklist and return the existing work items
clearWorkList :: EGS s [ClassId]
clearWorkList = do
    wl <- gets worklist
    modify (\egraph -> egraph { worklist = [] })
    return wl

getNode :: Ord s => s -> EGraph s -> ENode s
getNode i = (M.! i) . eNodes

-- | Get an e-class from an e-graph given its e-class id
--
-- Returns the canonical id of the class and the class itself
--
-- We'll find its canonical representation and then get it from the e-classes map
--
-- Invariant: The e-class always exists.
getClass :: ClassId -> EGraph s -> (ClassId, EClass s)
getClass cid egraph =
    let canon_id = find cid egraph
     in (canon_id, eClasses egraph IM.! canon_id)

-- | Extend the existing UnionFind equivalence classes with a new one and
-- return the new id
createUnionFindClass :: EGS s ClassId
createUnionFindClass = do
    uf <- gets eUnionFind
    let (new_id, new_uf) = makeNewSet uf
    modify (\egraph -> egraph { eUnionFind = new_uf })
    return new_id

-- | Merge two equivalent classes in the union find
mergeUnionFindClasses :: ClassId -> ClassId -> EGS s ClassId
mergeUnionFindClasses a b = do
    uf <- gets eUnionFind
    let (new_id, new_uf) = unionSets a b uf
    modify (\egraph -> egraph { eUnionFind = new_uf })
    return new_id


modifyClasses :: (ClassIdMap (EClass s) -> ClassIdMap (EClass s)) -> EGS s ()
modifyClasses f = modify (\egraph -> egraph { eClasses = f (eClasses egraph) })

modifyNodesClasses :: (Map s ClassId -> Map s ClassId) -> EGS s ()
modifyNodesClasses f = modify (\egraph -> egraph { eNodesClasses = f (eNodesClasses egraph) })

modifyNodes :: (Map s (ENode s) -> Map s (ENode s)) -> EGS s ()
modifyNodes f = modify (\egraph -> egraph { eNodes = f (eNodes egraph) })

emptyEGraph :: EGraph s
emptyEGraph = EGraph emptyUF IM.empty M.empty M.empty []

getSize :: EGS s Int
getSize = sizeEGraph <$> get

sizeEGraph :: EGraph s -> Int
sizeEGraph (EGraph { eUnionFind = (RUF im) }) = IM.size im

