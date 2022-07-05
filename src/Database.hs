{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Tree as T

import GHC.Data.TrieMap

import EGraph.EClass
import EGraph.ENode
import EGraph

newtype Fix f = In { out :: f (Fix f) }
deriving instance Show (f (Fix f)) => Show (Fix f)

-- | Query variable
type Var = String

-- | An Atom ... in pattern ... is R_f(v, v1, ..., vk), so we define it as a
-- functor ast over pattern variables + the additional var for the e-class id
data Atom lang = Atom Var (lang Var)
deriving instance Show (lang Var) => Show (Atom lang)

data Query lang = Query [Var] [Atom lang]
deriving instance Show (lang Var) => Show (Query lang)

-- | Database made of trie maps for each relation. Each relation is uniquely
-- identified by the expressions modulo children expressions (hence @lang ()@)
newtype Database lang = DB (Map (lang ()) (Fix ClassIdMap))
deriving instance (Show (Fix ClassIdMap), Show (lang ())) => Show (Database lang)

-- insert :: Ord (ENode lang) => (ClassId, ENode lang) -> Database lang -> Database lang
-- insert (c,e) (DB m) = DB $ M.insert e (c,e) m

-- | Take a query and produce substitutions from query variables to actual
-- classid
genericJoin :: Foldable lang => Database lang -> Query lang -> [(Var, ClassId)]
genericJoin _ (Query [] _) = []
genericJoin d (Query (x:xs) atoms) =
    []
    where
        atomsWithX = filter (x `elemOfAtom`) atoms
        domainX = intersectAtoms d atomsWithX


elemOfAtom :: Foldable lang => Var -> Atom lang -> Bool
elemOfAtom x (Atom v l) = x == v || x `elem` toList l

intersectAtoms :: Database lang -> [Atom lang] -> [ClassId]
intersectAtoms = undefined
