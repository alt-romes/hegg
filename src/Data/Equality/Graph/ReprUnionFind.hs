{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-|

Union-find like data structure that defines equivalence classes of e-class ids

-}
module Data.Equality.Graph.ReprUnionFind where

import Data.Equality.Graph.Classes.Id
import qualified Data.Equality.Utils.IntToIntMap as IIM
import qualified Data.IntMap as IM

import GHC.Exts ((+#), Int(..), Int#)

-- | A union find for equivalence classes of e-class ids.
--
-- Particularly, there's no value associated with identifier, so this union find serves only to find the representative of an e-class id
--
-- e.g. @FUF $ fromList [(y, Canonical), (x, Represented y)]@
data ReprUnionFind = RUF !IIM.IntToIntMap -- ^ Map every id to either 0# (meaning its the representative) or to another int# (meaning its represented by some other id)
                         RUFSize -- ^ Counter for new ids
                         !(IM.IntMap [ClassId]) -- ^ Mapping from an id to all its children: This is used for "rebuilding" (compress all paths) when merging. Its a hashcons?
                         -- [ClassId] -- ^ Ids that can be safely deleted after the e-graph is rebuilt

instance Show ReprUnionFind where
  show (RUF _ s _) = "RUF with size:" <> show (I# s)

type RUFSize = Int#

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
emptyUF = RUF IIM.Nil
              1# -- Must start with 1# since 0# means "Canonical"
              mempty
              -- mempty

-- | Create a new e-class id in the given 'ReprUnionFind'.
makeNewSet :: ReprUnionFind
           -> (ClassId, ReprUnionFind) -- ^ Newly created e-class id and updated 'ReprUnionFind'
makeNewSet (RUF im si hc) = ((I# si), RUF (IIM.insert si 0# im) ((si +# 1#)) (IM.insert (I# si) mempty hc))
{-# SCC makeNewSet #-}

-- | Union operation of the union find.
--
-- Given two leader ids, unions the two eclasses making @a@ the leader.(that is,
-- @b@ is represented by @a@
unionSets :: ClassId -> ClassId -> ReprUnionFind -> (ClassId, ReprUnionFind)
unionSets a@(I# a#) b@(I# b#) (RUF im si hc) = (a, RUF (IIM.insert b# a# im) si hc)
  where
    represented_by_b = hc IM.! b
    -- Overwrite previous id of b (which should be 0#) with new representative (a)
    -- AND "rebuild" all nodes represented by b by making them represented directly by a
    new_im = {-# SCC "rebuild_im" #-} IIM.unliftedFoldr (\(I# x) -> IIM.insert x a#) (IIM.insert b# a# im) represented_by_b
    new_hc = {-# SCC "adjust_hc" #-} IM.adjust ((b:) . (represented_by_b <>)) a (IM.delete b hc)
{-# SCC unionSets #-}

-- | Find the canonical representation of an e-class id
findRepr :: ClassId -> ReprUnionFind -> ClassId
findRepr (I# v0) (RUF m' _ _) =
  let
    go :: IIM.IntToIntMap -> Int# -> Int#
    go m v =
      case {-# SCC "findRepr_TAKE" #-} m IIM.! v of
        0# -> v        -- v is Canonical (0# means canonical)
        x  -> go m x   -- v is Represented by x
   in I# (go m' v0)

-- ROMES:TODO: Path compression in immutable data structure? Is it worth
-- the copy + threading?
--
-- ANSWER: According to my tests, findRepr is always quite shallow, going only
-- (from what I saw) until, at max, depth 3!
--
-- When using the ad-hoc path compression in `unionSets`, the depth of
-- recursion never even goes above 1!
{-# SCC findRepr #-}

-- -- | Delete nodes that have been merged after e-graph has been rebuilt
-- rebuildUF :: ReprUnionFind -> ReprUnionFind
-- rebuildUF (RUF m' a b dl) = RUF (IIM.unliftedFoldr (\(I# x) -> IIM.delete x) m' dl) a b mempty
