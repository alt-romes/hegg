{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE KindSignatures #-}
module Data.Equality.Graph where

import Data.Equality.Graph.Classes.Id
import {-# SOURCE #-} Data.Equality.Graph.Classes (EClass)

type role EGraph nominal
data EGraph (l :: * -> *)

getClass :: ClassId -> EGraph s -> (ClassId, EClass s)

setClass :: EGraph s -> ClassId -> EClass s -> EGraph s

