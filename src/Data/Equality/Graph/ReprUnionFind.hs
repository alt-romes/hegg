{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-|

Union-find-like data structure that defines equivalence classes of e-class ids.

-}
module Data.Equality.Graph.ReprUnionFind
  ( ReprUnionFind
  , emptyUF
  , makeNewSet
  , unionSets
  , findRepr
  ) where

import Data.Equality.Graph.Classes.Id

#if __GLASGOW_HASKELL__ >= 902

import qualified Data.Equality.Utils.IntToIntMap as IIM
import GHC.Exts ((+#), Int(..), Int#)

type RUFSize = Int#

-- | A union find for equivalence classes of e-class ids.
data ReprUnionFind = RUF IIM.IntToIntMap -- ^ Map every id to either 0# (meaning its the representative) or to another int# (meaning its represented by some other id)
                         RUFSize         -- ^ Counter for new ids

                         -- !(IM.IntMap [ClassId]) -- ^ Mapping from an id to all its children: This is used for "rebuilding" (compress all paths) when merging. Its a hashcons?
                         -- [ClassId] -- ^ Ids that can be safely deleted after the e-graph is rebuilt
#else

import qualified Data.IntMap.Internal as IIM (IntMap(..))
import qualified Data.IntMap.Strict as IIM

-- | A union find for equivalence classes of e-class ids.
data ReprUnionFind = RUF (IIM.IntMap Int)    -- ^ Map every id to either 0# (meaning its the representative) or to another int# (meaning its represented by some other id)
                         {-# UNPACK #-} !Int -- ^ Counter for new ids

#endif

-- Note that there's no value associated with identifier, so this union find
-- serves only to find the representative of an e-class id

instance Show ReprUnionFind where
  show (RUF _ _) = "Warning: Incomplete show: ReprUnionFind"

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
emptyUF :: ReprUnionFind
-- TODO: If I can make an instance of 'ReprUnionFind' for Monoid(?), this is 'mempty'
emptyUF = RUF IIM.Nil
#if __GLASGOW_HASKELL__ >= 902
              1# -- Must start with 1# since 0# means "Canonical"
#else
              1
#endif

-- | Create a new e-class id in the given 'ReprUnionFind'.
makeNewSet :: ReprUnionFind
           -> (ClassId, ReprUnionFind) -- ^ Newly created e-class id and updated 'ReprUnionFind'
#if __GLASGOW_HASKELL__ >= 902
makeNewSet (RUF im si) = ((I# si), RUF (IIM.insert si 0# im) ((si +# 1#)))
#else
makeNewSet (RUF im si) = (si, RUF (IIM.insert si 0 im) (si + 1))
#endif

-- | Union operation of the union find.
--
-- Given two leader ids, unions the two eclasses making @a@ the leader, that
-- is, @b@ is now represented by @a@
unionSets :: ClassId                  -- ^ E-class id @a@
          -> ClassId                  -- ^ E-class id @b@
          -> ReprUnionFind            -- ^ Union-find containing @a@ and @b@
          -> (ClassId, ReprUnionFind) -- ^ The new leader (always @a@) and the updated union-find
#if __GLASGOW_HASKELL__ >= 902
unionSets a@(I# a#) (I# b#) (RUF im si) = (a, RUF (IIM.insert b# a# im) si)
#else
unionSets a b (RUF im si) = (a, RUF (IIM.insert b a im) si)
#endif
  -- where
    -- represented_by_b = hc IM.! b
    -- -- Overwrite previous id of b (which should be 0#) with new representative (a)
    -- -- AND "rebuild" all nodes represented by b by making them represented directly by a
    -- new_im = IIM.unliftedFoldr (\(I# x) -> IIM.insert x a#) (IIM.insert b# a# im) represented_by_b
    -- new_hc = IM.adjust ((b:) . (represented_by_b <>)) a (IM.delete b hc)

-- | Find the canonical representation of an e-class id
findRepr :: ClassId -> ReprUnionFind
         -> ClassId -- ^ The found canonical representation
#if __GLASGOW_HASKELL__ >= 902
findRepr v@(I# v#) (RUF m s) =
  case m IIM.! v# of
    0# -> v
    x  -> findRepr (I# x) (RUF m s)
#else
findRepr v (RUF m s) =
  case m IIM.! v of
    0 -> v
    x -> findRepr x (RUF m s)
#endif

-- ROMES:TODO: Path compression in immutable data structure? Is it worth
-- the copy + threading?
--
-- ANSWER: According to my tests, findRepr is always quite shallow, going only
-- (from what I saw) until, at max, depth 3!
--
-- When using the ad-hoc path compression in `unionSets`, the depth of
-- recursion never even goes above 1!


-- {-# RULES
--    "union/find" forall a b x im. findRepr (I# b) (RUF (IIM.insert b a im) x) = I# a
--   #-}

-- -- | Delete nodes that have been merged after e-graph has been rebuilt
-- rebuildUF :: ReprUnionFind -> ReprUnionFind
-- rebuildUF (RUF m' a b dl) = RUF (IIM.unliftedFoldr (\(I# x) -> IIM.delete x) m' dl) a b mempty
