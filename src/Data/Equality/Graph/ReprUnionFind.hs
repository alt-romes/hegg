{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-|

Union-find like data structure that defines equivalence classes of e-class ids

-}
module Data.Equality.Graph.ReprUnionFind where

import qualified Data.IntMap.Strict as IMS

import Data.Equality.Graph.Classes.Id

-- | A union find for equivalence classes of e-class ids.
--
-- Particularly, there's no value associated with identifier, so this union find serves only to find the representative of an e-class id
--
-- e.g. @FUF $ fromList [(y, Canonical), (x, Represented y)]@
data ReprUnionFind = RUF !(ClassIdMap Repr) {-# UNPACK #-} !RUFSize
    deriving Show

type RUFSize = Int

-- | An @id@ can be represented by another @id@ or be canonical, meaning it
-- represents itself.
--
-- @(x, Represented y)@ would mean x is represented by y
-- @(x, Canonical)@ would mean x is canonical -- represents itself
newtype Repr
  = Represented { unRepr :: ClassId } -- ^ @Represented x@ is represented by @x@
--   | Canonical -- ^ @Canonical x@ is the canonical representation, meaning @find(x) == x@
  deriving Show

-- | The empty 'ReprUnionFind'.
--
-- TODO: Instance of 'ReprUnionFind' as Monoid, this is 'mempty'
emptyUF :: ReprUnionFind
emptyUF = RUF mempty 0

-- | Create a new e-class id in the given 'ReprUnionFind'.
makeNewSet :: ReprUnionFind
           -> (ClassId, ReprUnionFind) -- ^ Newly created e-class id and updated 'ReprUnionFind'
makeNewSet (RUF im si) = (si, RUF (IMS.insert si (Represented si) im) (si+1))

-- | Union operation of the union find.
--
-- Given two leader ids, unions the two eclasses making @a@ the leader.(that is,
-- @b@ is represented by @a@
unionSets :: ClassId -> ClassId -> ReprUnionFind -> (ClassId, ReprUnionFind)
unionSets !a !b (RUF im si) = (a, RUF (IMS.insert b (Represented a) im) si)

-- | Find the canonical representation of an e-class id
findRepr :: ClassId -> ReprUnionFind -> ClassId
findRepr !v uf@(RUF m _) =
  case m IMS.! v of
    Represented x ->
      if x == v
         then v -- v is Canonical
         else findRepr x uf -- v is Represented by x
  -- ROMES:TODO: Path compression in immutable data structure? Is it worth
  -- the copy + threading?
{-# SCC findRepr #-}

