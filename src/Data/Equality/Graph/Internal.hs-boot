{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Data.Equality.Graph.Internal where

import Data.Kind

type role EGraph nominal nominal
type EGraph :: Type -> (Type -> Type) -> Type
data EGraph a l
