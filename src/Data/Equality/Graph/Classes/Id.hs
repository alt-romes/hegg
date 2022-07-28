{-|

Types for e-class ids

-}
module Data.Equality.Graph.Classes.Id
    ( ClassId
    , ClassIdMap
    ) where

import qualified Data.IntMap.Strict as IM

-- | Type for e-class ids
type ClassId = Int

-- | A map from e-class ids to values
type ClassIdMap = IM.IntMap
