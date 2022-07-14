{-| Equivalence classes ids -}
module Data.Equality.Graph.Classes.Id
    ( ClassId
    , ClassIdMap
    ) where

import qualified Data.IntMap as IM

-- | Class Id
type ClassId = Int

-- | Map from class ids to values
type ClassIdMap = IM.IntMap
