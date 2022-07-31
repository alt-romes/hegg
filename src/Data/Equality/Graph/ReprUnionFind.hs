{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-|

Union-find like data structure that defines equivalence classes of e-class ids

-}
module Data.Equality.Graph.ReprUnionFind where

import Data.Equality.Graph.Classes.Id
import qualified Data.Equality.Utils.IntToIntMap as IIM

import GHC.Exts ((==#), (+#), Int(..), Int#)

-- | A union find for equivalence classes of e-class ids.
--
-- Particularly, there's no value associated with identifier, so this union find serves only to find the representative of an e-class id
--
-- e.g. @FUF $ fromList [(y, Canonical), (x, Represented y)]@
data ReprUnionFind = RUF !IIM.IntToIntMap {-# UNPACK #-} !RUFSize

instance Show ReprUnionFind where
  show (RUF _ s) = "RUF with size:" <> show s

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
emptyUF = RUF IIM.Nil 0

-- | Create a new e-class id in the given 'ReprUnionFind'.
makeNewSet :: ReprUnionFind
           -> (ClassId, ReprUnionFind) -- ^ Newly created e-class id and updated 'ReprUnionFind'
makeNewSet (RUF im (I# si)) = ((I# si), RUF (IIM.insert si (si) im) (I# (si +# 1#)))

-- | Union operation of the union find.
--
-- Given two leader ids, unions the two eclasses making @a@ the leader.(that is,
-- @b@ is represented by @a@
unionSets :: ClassId -> ClassId -> ReprUnionFind -> (ClassId, ReprUnionFind)
unionSets (I# a) (I# b) (RUF im si) = (I# a, RUF (IIM.insert b (a) im) si)

-- | Find the canonical representation of an e-class id
findRepr :: ClassId -> ReprUnionFind -> ClassId
findRepr (I# v0) (RUF m _) = I# (go v0)
  where
    go :: Int# -> Int#
    go v =
      case m IIM.! v of
        x ->
          case x ==# v of
            0# -> go x   -- v is Represented by x
            _  -> v      -- v is Canonical


  -- ROMES:TODO: Path compression in immutable data structure? Is it worth
  -- the copy + threading?
{-# SCC findRepr #-}
