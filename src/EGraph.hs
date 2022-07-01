{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module EGraph where

import Data.Bifunctor

import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

-- | E-graph stateful computation
type EGS s i = State (EGraph s i)

runEGS :: EGraph s i -> EGS s i a -> (a, EGraph s i)
runEGS = flip runState

type ClassId = Int
type ClassIdMap = IM.IntMap

-- | E-graph
--
-- @s@ for the e-node term
-- @nid@ type of e-node ids
data EGraph s nid = EGraph
    { eUnionFind    :: ReprUnionFind -- ^ Union find like structure to find canonical representation of an e-class id
    , eClasses      :: ClassIdMap (EClass nid) -- ^ Map (canonical?) e-class ids to their e-classes
    , eNodesClasses :: Map nid ClassId -- ^ Hashcons maps ids of all canonical e-nodes to their e-class ids
    , eNodes        :: Map nid (ENode s nid) -- ^ Map e-node ids to e-nodes
    , worklist      :: [ClassId] -- ^ e-class ids that need to be upward merged
    }
    deriving Show

-- | E-graph
--
-- @cid@ type of e-class ids
-- @nid@ type of e-node ids
data EClass nid = EClass
    { eClassId :: {-# UNPACK #-} !ClassId -- ^ E-class Id
    , eClassNodes :: S.Set nid -- ^ E-nodes in this class
    , eClassParents :: [(nid, ClassId)] -- ^ E-nodes which are parents of (reference) this e-class and their e-class ids
    }
    deriving Show

-- | E-node
--
-- @s@ type of e-node term
-- @nid@ type of e-node ids
data ENode s nid = ENode
    { eNodeId :: !nid
    , eNodeTerm :: s
    , eNodeChildren :: [ClassId]
    }
    deriving Show

instance Eq s => Eq (ENode s nid) where
    (ENode _ s xs) == (ENode _ s' xs') = s == s' && xs == xs'

instance (Eq s, Ord nid) => Ord (ENode s nid) where
    (ENode nid _ _) <= (ENode nid' _ _) = nid <= nid'

-- | Add an e-node to the e-graph
add :: Ord nid => ENode s nid -> EGS s nid ClassId
add uncanon_e = do
    egraph@(EGraph { eNodesClasses = encls }) <- get
    let new_en@(ENode enode_id t children) = canonicalize uncanon_e egraph
    case M.lookup enode_id encls of
      Just canon_enode_id -> return canon_enode_id
      Nothing -> do
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
merge :: ClassId -> ClassId -> EGS s i ClassId
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
canonicalize :: ENode s nid -> EGraph s nid -> ENode s nid
canonicalize (ENode i x ls) egraph = ENode i x (map (flip find egraph) ls)

-- | Find the canonical representation of an e-class id in the e-graph
-- Invariant: The e-class id always exists.
find :: ClassId -> EGraph s nid -> ClassId
find cid = unsafeUnpack . findRepr cid . eUnionFind
    where
        unsafeUnpack Nothing  = error $ "The impossible happened: Couldn't find representation of e-node " <> show cid
        unsafeUnpack (Just x) = x

rebuild :: (Eq s, Ord i) => EGS s i ()
rebuild = do
    eg <- get
    wl <- clearWorkList
    let todo = S.fromList $ map (flip find eg) wl
    forM_ todo repair

repair :: (Eq s, Ord i) => ClassId -> EGS s i ()
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
            go :: (Eq s, Ord i) => [(i, ClassId)] -> Map (ENode s i) ClassId -> EGS s i (Map (ENode s i) ClassId)
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



-- | Add a singleton e-class with the e-node (first arg) to the e-graph
singletonClass :: Ord nid => ENode s nid -> EGS s nid ClassId
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

addToWorklist :: ClassId -> EGS s nid ()
addToWorklist i =
    modify (\egraph -> egraph { worklist = i:worklist egraph })

-- | Clear the e-graph worklist and return the existing work items
clearWorkList :: EGS s nid [ClassId]
clearWorkList = do
    wl <- gets worklist
    modify (\egraph -> egraph { worklist = [] })
    return wl

getNode :: Ord nid => nid -> EGraph s nid -> ENode s nid
getNode i = (M.! i) . eNodes

-- | Get an e-class from an e-graph given its e-class id
--
-- Returns the canonical id of the class and the class itself
--
-- We'll find its canonical representation and then get it from the e-classes map
--
-- Invariant: The e-class always exists.
getClass :: ClassId -> EGraph s nid -> (ClassId, EClass nid)
getClass cid egraph =
    let canon_id = find cid egraph
     in (canon_id, eClasses egraph IM.! canon_id)

-- | Extend the existing UnionFind equivalence classes with a new one and
-- return the new id
createUnionFindClass :: EGS s i ClassId
createUnionFindClass = do
    uf <- gets eUnionFind
    let (new_id, new_uf) = makeNewSet uf
    modify (\egraph -> egraph { eUnionFind = new_uf })
    return new_id

-- | Merge two equivalent classes in the union find
mergeUnionFindClasses :: ClassId -> ClassId -> EGS s i ClassId
mergeUnionFindClasses a b = do
    uf <- gets eUnionFind
    let (new_id, new_uf) = unionSets a b uf
    modify (\egraph -> egraph { eUnionFind = new_uf })
    return new_id


modifyClasses :: (ClassIdMap (EClass nid) -> ClassIdMap (EClass nid)) -> EGS s nid ()
modifyClasses f = modify (\egraph -> egraph { eClasses = f (eClasses egraph) })

modifyNodesClasses :: (Map nid ClassId -> Map nid ClassId) -> EGS s nid ()
modifyNodesClasses f = modify (\egraph -> egraph { eNodesClasses = f (eNodesClasses egraph) })

modifyNodes :: (Map nid (ENode s nid) -> Map nid (ENode s nid)) -> EGS s nid ()
modifyNodes f = modify (\egraph -> egraph { eNodes = f (eNodes egraph) })

emptyEGraph :: EGraph s nid
emptyEGraph = EGraph emptyUF IM.empty M.empty M.empty []

-- | A union find in which the elements are the same as the keys, meaning we
-- keep only track of the representation of the @id@
--
-- e.g. @WUF $ fromList [(y, Canonical), (x, Represented y)]@
newtype ReprUnionFind = RUF (ClassIdMap Repr)
    deriving Show

-- | An @id@ can be represented by another @id@ or be canonical, meaning it
-- represents itself.
--
-- @(x, Represented y)@ would mean x is represented by y
-- @(x, Canonical)@ would mean x is canonical -- represents itself
data Repr
  = Represented {-# UNPACK #-} !ClassId -- ^ @Represented x@ is represented by @x@
  | Canonical -- ^ @Canonical x@ is the canonical representation, meaning @find(x) == x@
  deriving Show

emptyUF :: ReprUnionFind
emptyUF = RUF IM.empty

makeNewSet :: ReprUnionFind -> (ClassId, ReprUnionFind)
makeNewSet (RUF im) = (new_id, RUF $ IM.insert new_id Canonical im)
    where
        new_id = IM.size im

-- | Union operation of the union find.
--
-- Given two leader ids, unions the two eclasses making root1 the leader.
unionSets :: ClassId -> ClassId -> ReprUnionFind -> (ClassId, ReprUnionFind)
unionSets a b (RUF im) = (a, RUF $ IM.update (\Canonical -> Just $ Represented a) b im)

-- | Find the canonical representation of an id
findRepr :: ClassId -> ReprUnionFind -> Maybe ClassId
findRepr v (RUF m) =
    -- ROMES:TODO: Path compression in immutable data structure? Is it worth
    -- the copy + threading?
    IM.lookup v m >>= \case
        Represented x -> findRepr x (RUF m)
        Canonical     -> Just v

