{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
module Data.Equality.Graph.Classes where

type role EClass nominal
data EClass (l :: * -> *)
