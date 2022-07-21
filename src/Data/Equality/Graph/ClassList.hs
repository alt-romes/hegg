{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- Show
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Equality.Graph.ClassList
    ( ClassList(unClassList)
    , insert
    , lookup
    , delete
    , size
    , (!)
    ) where

import Prelude hiding (lookup)

import Data.Functor.Classes
import qualified Data.IntMap as IM
import Data.Equality.Graph.Classes
import Data.Equality.Analysis

newtype ClassList l = CL { unClassList :: ClassIdMap (EClass l) } -- ^ Map canonical e-class ids to their e-classes
    deriving (Semigroup, Monoid)

deriving instance (Show1 l, Show (Domain l)) => Show (ClassList l)

insert :: ClassId' 'Canon -> EClass l -> ClassList l -> ClassList l
insert (ClassId cid) c (CL m) = CL $ IM.insert cid c m
{-# INLINE insert #-}

lookup :: ClassId' 'Canon -> ClassList l -> Maybe (EClass l)
lookup (ClassId cid) (CL m) = cid `IM.lookup` m
{-# INLINE lookup #-}

delete :: ClassId' 'Canon -> ClassList l -> ClassList l
delete (ClassId cid) (CL m) = CL $ IM.delete cid m
{-# INLINE delete #-}

size :: ClassList l -> Int
size = IM.size . unClassList
{-# INLINE size #-}

(!) :: ClassList l -> ClassId' 'Canon -> EClass l
(!) (CL m) (ClassId i) = m IM.! i
{-# INLINE (!) #-}
