{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Data.Equality.Graph.Internal where

import Data.Kind

type EGraph :: (Type -> Type) -> Type
type role EGraph nominal
data EGraph l
