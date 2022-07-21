{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
module Data.Equality.Graph.ClassList where

import Data.Kind

type role ClassList nominal
data ClassList (l :: Type -> Type)
