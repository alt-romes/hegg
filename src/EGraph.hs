{-# LANGUAGE LambdaCase #-}
module EGraph where

import Data.Map (Map)
import qualified Data.Map    as M
import qualified Data.Set    as S

data EGraph s id = EGraph
    { unionFind :: ReprUnionFind id
    , eClasses  :: Map id (EClass s id)
    , eNodes    :: Map (ENode s id) id
    }

newtype EClass s id = EClass (S.Set (ENode s id))

data ENode s id = ENode s [id]

-- | A union find in which the elements are the same as the keys, meaning we
-- keep only track of the representation of the @id@
--
-- e.g. @WUF $ fromList [(y, Canonical), (x, Represented y)]@
newtype ReprUnionFind id = RUF (Map id (Repr id))

-- | An @id@ can be represented by another @id@ or be canonical, meaning it
-- represents itself.
--
-- @(x, Represented y)@ would mean x is represented by y
-- @(x, Canonical)@ would mean x is canonical -- represents itself
data Repr id
  = Represented {-# UNPACK #-} !id -- ^ @Represented x@ is represented by @x@
  | Canonical -- ^ @Canonical x@ is the canonical representation, meaning @find(x) == x@
  deriving (Show)

emptyUF :: ReprUnionFind id
emptyUF = RUF M.empty

-- | Find the canonical representation of an id
findRepr :: Ord id => id -> ReprUnionFind id -> Maybe id
findRepr v (RUF m) =
    -- ROMES:TODO: Path compression in immutable data structure? Is it worth
    -- the copy + threading?
    M.lookup v m >>= \case
        Represented x -> findRepr x (RUF m)
        Canonical     -> Just v
