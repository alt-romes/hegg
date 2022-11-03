{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
module Data.Equality.Graph.Classes where

import Data.Kind

type role EClass representational nominal
data EClass a (l :: Type -> Type)
