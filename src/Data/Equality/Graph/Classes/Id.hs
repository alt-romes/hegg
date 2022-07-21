{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-|

Types for e-class ids

Also defines 'Canonicality' that allows one to define whether the e-class id or
e-node is canonical or not. This is relevant in operations that depend on
things being canonical or not.

-}
module Data.Equality.Graph.Classes.Id
    ( ClassId'(..)
    , ClassId
    , ClassIdMap
    , Canonicality(..)
    , fromCanon
    ) where

import qualified Data.IntMap as IM

data Canonicality = Canon | Uncanon

-- | Take ClassId from Canon ClassId
-- (Lose canonicality)
--
-- When we union two canonical ids, one loses its canonicality!
fromCanon :: ClassId' 'Canon -> ClassId
fromCanon (ClassId x) = x
{-# INLINE fromCanon #-}

-- | Typed e-class ids (wiith canonicality)
newtype ClassId' (k :: Canonicality) = ClassId { unwrapId :: Int }
    deriving (Eq, Ord, Show)

-- | Type synonym for e-class ids
type ClassId = Int

-- | A map from e-class ids to values
type ClassIdMap = IM.IntMap
