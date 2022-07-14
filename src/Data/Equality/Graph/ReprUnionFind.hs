{-# LANGUAGE LambdaCase #-}
module Data.Equality.Graph.ReprUnionFind where

import qualified Data.IntMap as IM

import Data.Equality.Graph.Classes

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
  = Represented {-# UNPACK #-} !ClassId -- ^ @Represented x@ is represented by @x@
  | Canonical -- ^ @Canonical x@ is the canonical representation, meaning @find(x) == x@
  deriving Show

emptyUF :: ReprUnionFind
emptyUF = RUF mempty

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

