{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-|

Union-find like data structure that defines equivalence classes of e-class ids

-}
module Data.Equality.Graph.ReprUnionFind where

import qualified Data.IntMap as IM

import Data.Equality.Graph.Classes.Id

-- | A union find for equivalence classes of e-class ids.
--
-- Particularly, there's no value associated with identifier, so this union find serves only to find the representative of an e-class id
--
-- e.g. @FUF $ fromList [(y, Canonical), (x, Represented y)]@
newtype ReprUnionFind = RUF (ClassIdMap Repr)
    deriving Show

-- | An @id@ can be represented by another @id@ or be canonical, meaning it
-- represents itself.
--
-- @(x, Represented y)@ would mean x is represented by y
-- @(x, Canonical)@ would mean x is canonical -- represents itself
data Repr
  = Represented {-# UNPACK #-} !(ClassId' 'Uncanon) -- ^ @Represented x@ is represented by @x@, an uncanonicalized id
  | Canonical -- ^ @Canonical x@ is the canonical representation, meaning @find(x) == x@
  deriving Show

-- | The empty 'ReprUnionFind'.
--
-- TODO: Instance of 'ReprUnionFind' as Monoid, this is 'mempty'
emptyUF :: ReprUnionFind
emptyUF = RUF mempty

-- | Create a new e-class id in the given 'ReprUnionFind'.
makeNewSet :: ReprUnionFind
           -> (ClassId' 'Canon, ReprUnionFind) -- ^ Newly created e-class id and updated 'ReprUnionFind'
makeNewSet (RUF im) = (ClassId new_id, RUF $ IM.insert new_id Canonical im)
    where
        new_id = IM.size im

-- | Union operation of the union find.
--
-- Given two leader ids, unions the two eclasses making root1 the leader.
--
-- TODO: Remove panic with types! Lookup with Uncanon should always result in
-- Represented and vice versa
unionSets :: ClassId' 'Canon -> ClassId' 'Canon -> ReprUnionFind -> (ClassId' 'Canon, ReprUnionFind)
unionSets a (ClassId b) (RUF im) =
    ( a
    , RUF $ IM.update
        (\case Canonical     -> pure (Represented (ClassId $ fromCanon a))
               Represented _ -> error "impossible: b is proved to be canon")
        b im
    )

-- | Find the canonical representation of an e-class id
--
-- TODO: Should be just from uncanon nodes?
findRepr :: ClassId' k -> ReprUnionFind -> Maybe (ClassId' 'Canon)
findRepr (ClassId v) (RUF m) =
    -- ROMES:TODO: Path compression in immutable data structure? Is it worth
    -- the copy + threading?
    IM.lookup v m >>= \case
        Represented x -> findRepr x (RUF m)
        Canonical     -> Just (ClassId v)
