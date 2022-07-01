{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module EGraph where

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

-- | Add an e-node to the e-graph
add :: Ord nid => ENode s nid -> EGS s nid ClassId
add uncanon_e = do
    egraph@(EGraph _ _ encls _) <- get
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
-- merge :: ClassId -> ClassId -> EGraph s i -> EGraph s i
-- merge a b egraph =
--     if find a == find b
            

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

modifyClasses :: (ClassIdMap (EClass nid) -> ClassIdMap (EClass nid)) -> EGS s nid ()
modifyClasses f = modify (\egraph -> egraph { eClasses = f (eClasses egraph) })

modifyNodesClasses :: (Map nid ClassId -> Map nid ClassId) -> EGS s nid ()
modifyNodesClasses f = modify (\egraph -> egraph { eNodesClasses = f (eNodesClasses egraph) })

modifyNodes :: (Map nid (ENode s nid) -> Map nid (ENode s nid)) -> EGS s nid ()
modifyNodes f = modify (\egraph -> egraph { eNodes = f (eNodes egraph) })

emptyEGraph :: EGraph s nid
emptyEGraph = EGraph emptyUF IM.empty M.empty M.empty

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

-- | Find the canonical representation of an id
findRepr :: ClassId -> ReprUnionFind -> Maybe ClassId
findRepr v (RUF m) =
    -- ROMES:TODO: Path compression in immutable data structure? Is it worth
    -- the copy + threading?
    IM.lookup v m >>= \case
        Represented x -> findRepr x (RUF m)
        Canonical     -> Just v

