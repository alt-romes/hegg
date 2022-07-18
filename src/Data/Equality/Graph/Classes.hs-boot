{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
module Data.Equality.Graph.Classes where

import Data.Kind

type role EClass nominal
data EClass (l :: Type -> Type)
