{-# LANGUAGE TypeFamilies #-}
module EGraph.ENode where

import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S

import Data.Foldable

import EGraph.EClass

-- | E-node
--
-- An E-node is a function symbol paired with a list of children e-classes.
-- 
-- We define an e-node to be the base functor of some recursive data type
-- parametrized over ClassId, i.e. all recursive fields are rather e-class ids.
--
-- When @l@ is an expression-like data type, @ENode l = l ClassId@ means every
-- recursive field (so, every argument passed to this expr) is a 'ClassId'
-- rather than an explicit expression
type ENode l = l ClassId

children :: Foldable l => ENode l -> [ClassId]
children = toList

-- TODO: type class with associated type family "Operator" for the token that identifies the operator ...
-- Maybe use it instead of the functors? aber the functors look so natural..
class ELang lang where

    -- | Token that uniquely identifies operator
    type Operator lang


