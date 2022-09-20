{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
module Data.Equality.Graph.Classes where

import Data.Kind

type role EClass nominal nominal
data EClass a (l :: Type -> Type)
