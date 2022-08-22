{-|

Type synonyms for e-class ids.

-}
module Data.Equality.Graph.Classes.Id
    ( ClassId
    , ClassIdMap
    ) where

import qualified Data.IntMap.Strict as IM

-- | Type synonym for e-class ids
type ClassId = Int

-- | Type synonym for a map from e-class ids to values
type ClassIdMap = IM.IntMap
