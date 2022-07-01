{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module EGraph where

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

type SEG s i = State (EGraph s i)

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

-- | E-graph
--
-- @cid@ type of e-class ids
-- @nid@ type of e-node ids
data EClass nid = EClass
    { eClassId :: {-# UNPACK #-} !ClassId -- ^ E-class Id
    , eClassNodes :: S.Set nid -- ^ E-nodes in this class
    , eClassParents :: [(nid, ClassId)] -- ^ E-nodes which are parents of (reference) this e-class and their e-class ids
    }

-- | E-node
--
-- @s@ type of e-node term
-- @nid@ type of e-node ids
data ENode s nid = ENode
    { eNodeId :: !nid
    , eNodeTerm :: s
    , eNodeChildren :: [ClassId]
    }

-- | Add an e-node to the e-graph
add :: Ord nid => ENode s nid -> EGraph s nid -> (ClassId, EGraph s nid)
add uncanon_e egraph@(EGraph _ ecls encls _) = do
    let new_en@(ENode enode_id t children) = canonicalize uncanon_e egraph
    case M.lookup enode_id encls of
      Just canon_enode_id -> (canon_enode_id, egraph)
      Nothing ->
        let
            -- | Add new singleton e-class with the e-node
            (new_eclass_id, new_egraph) = singletonEClass new_en egraph
            -- Update e-classes by going through all e-node children and adding
            -- to the e-class parents the new e-node and its e-class id
            new_ecls = foldr updateChild ecls children
            -- | Update canonical e-class of child e-class
            updateChild eclass_id im = IM.update (\e_class -> Just $ e_class { eClassParents = (enode_id, new_eclass_id):(eClassParents e_class) }) (find eclass_id new_egraph) im
            new_enodesclasses = M.insert enode_id new_eclass_id (eNodesClasses new_egraph)
         in
            (new_eclass_id, new_egraph { eClasses = new_ecls
                                       , eNodesClasses = new_enodesclasses })
            

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

-- | Add a singleton e-class with the e-node (first arg) to the e-graph
singletonEClass :: Ord nid => ENode s nid -> EGraph s nid -> (ClassId, EGraph s nid)
singletonEClass en (EGraph uf ecls encls ens) = (new_id, new_egraph)
    where
        node_id          = eNodeId en
        (new_id, new_uf) = makeNewSet uf
        -- New e-class stores the e-node id of the e-node
        new_eclass = EClass new_id (S.singleton node_id) []
        -- Add new e-class to existing e-classes
        new_eclasses = IM.insert new_id new_eclass ecls
        -- New e-node is added at its id to the e-nodes map
        new_enodes = M.insert node_id en ens
        new_egraph = EGraph new_uf new_eclasses encls new_enodes

-- | Get an e-class from an e-graph given its e-class id
--
-- Returns the canonical id of the class and the class itself
--
-- We'll find its canonical representation and then get it from the e-classes map
--
-- Invariant: The e-class always exists.
getEClass :: ClassId -> EGraph s nid -> (ClassId, EClass nid)
getEClass cid egraph =
    let canon_id = find cid egraph
     in (canon_id, eClasses egraph IM.! canon_id)

-- | Find the canonical representation of an e-class id in the e-graph
-- Invariant: The e-class id always exists.
find :: ClassId -> EGraph s nid -> ClassId
find cid = unsafeUnpack . findRepr cid . eUnionFind
    where
        unsafeUnpack Nothing  = error "The impossible happened: Couldn't find representation of e-node child"
        unsafeUnpack (Just x) = x

-- | A union find in which the elements are the same as the keys, meaning we
-- keep only track of the representation of the @id@
--
-- e.g. @WUF $ fromList [(y, Canonical), (x, Represented y)]@
newtype ReprUnionFind = RUF (ClassIdMap Repr)

-- | An @id@ can be represented by another @id@ or be canonical, meaning it
-- represents itself.
--
-- @(x, Represented y)@ would mean x is represented by y
-- @(x, Canonical)@ would mean x is canonical -- represents itself
data Repr
  = Represented {-# UNPACK #-} !ClassId -- ^ @Represented x@ is represented by @x@
  | Canonical -- ^ @Canonical x@ is the canonical representation, meaning @find(x) == x@
  deriving (Show)

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

