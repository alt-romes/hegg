{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module EGraph where

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

type EG s = State (EGraph s Int, Int)

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

-- add :: Ord cid => ENode s cid -> EGraph s cid nid -> (cid, EGraph s cid)
-- add (canonicalize -> e@(ENode i t ls)) egraph = do
--     case lookup i (eNodeClasses egraph) of
--       Just x -> (x, egraph)
--       Nothing ->
--           singletonEClass e



-- | Canonicalize an E-Node
--
-- Two e-nodes are equal when their canonical form is equal. Canonicalization
-- makes the list of e-class ids the e-node holds a list of canonical ids.
-- Meaning two seemingly different e-nodes might be equal when we figure out
-- that their e-class ids are represented by the same e-class canonical ids
--
-- canonicalize(f(a,b,c,...)) = f((find a), (find b), (find c),...)
canonicalize :: ENode s nid -> EGraph s nid -> ENode s nid
canonicalize (ENode i x ls) (eUnionFind -> uf) = ENode i x (map (unsafeUnpack . flip findRepr uf) ls)
    where
        unsafeUnpack Nothing  = error "The impossible happened: Couldn't find representation of e-node child"
        unsafeUnpack (Just x) = x

-- | Add a singleton e-class with the e-node (first arg) to the e-graph
singletonEClass :: Ord nid => ENode s nid -> EGraph s nid -> (ClassId, EGraph s nid)
singletonEClass e egraph = (new_id, new_egraph)
    where
        new_id     = (sizeUF . eUnionFind) egraph
        node_id    = eNodeId e
        -- New e-class stores the e-node id of the e-node
        new_eclass = EClass new_id (S.singleton node_id) []
        -- Add new e-class to existing e-classes
        new_eclasses = IM.insert new_id new_eclass (eClasses egraph)
        -- New e-node is added with its id to the e-graphs e-nodes map
        new_enodes = M.insert node_id e (eNodes egraph)
        new_egraph = egraph { eClasses = new_eclasses, eNodes = new_enodes }

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

sizeUF :: ReprUnionFind -> Int
sizeUF (RUF im) = IM.size im

-- | Find the canonical representation of an id
findRepr :: ClassId -> ReprUnionFind -> Maybe ClassId
findRepr v (RUF m) =
    -- ROMES:TODO: Path compression in immutable data structure? Is it worth
    -- the copy + threading?
    IM.lookup v m >>= \case
        Represented x -> findRepr x (RUF m)
        Canonical     -> Just v

