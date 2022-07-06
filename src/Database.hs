{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database where

import Data.List (intersect)
import Control.Monad

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Tree as T

-- import GHC.Data.TrieMap

import EGraph.EClass
import EGraph.ENode
import EGraph

newtype Fix f = In { out :: f (Fix f) }
deriving instance Show (f (Fix f)) => Show (Fix f)

-- | Query variable
type Var = String

data ClassIdOrVar = ClassId ClassId | Var Var
    deriving (Show, Eq)

-- | An Atom ... in pattern ... is R_f(v, v1, ..., vk), so we define it as a
-- functor ast over pattern variables + the additional var for the e-class id
data Atom lang = Atom ClassIdOrVar (lang ClassIdOrVar)
deriving instance Show (lang ClassIdOrVar) => Show (Atom lang)

data Query lang = Query [Var] [Atom lang]
deriving instance (Show (lang ClassIdOrVar), Show (lang Var)) => Show (Query lang)

-- | Database made of trie maps for each relation. Each relation is uniquely
-- identified by the expressions modulo children expressions (hence @lang ()@)
newtype Database lang = DB (Map (lang ()) (Fix ClassIdMap))
deriving instance (Show (Fix ClassIdMap), Show (lang ())) => Show (Database lang)

-- insert :: Ord (ENode lang) => (ClassId, ENode lang) -> Database lang -> Database lang
-- insert (c,e) (DB m) = DB $ M.insert e (c,e) m

-- | Take a query and produce substitutions from query variables to actual
-- classid
genericJoin :: (Ord (lang ()), Foldable lang, Functor lang) => Database lang -> Query lang -> [(Var, ClassId)]
genericJoin _ (Query [] _) = []
genericJoin d (Query (x:xs) atoms) = concatMap (\d_x -> (x,d_x):genericJoin d (Query xs (substitute x d_x atoms))) domainX
    where
        atomsWithX = filter (x `elemOfAtom`) atoms

        domainX :: [ClassId]
        domainX = intersectAtoms d atomsWithX

        substitute :: Functor lang => Var -> ClassId -> [Atom lang] -> [Atom lang]
        substitute r i = map $ \case
            Atom (Var v) l
              | v == r -> Atom (ClassId i) l
            Atom x l -> Atom x $ flip fmap l $ \case
                            Var vi
                              | vi == r -> ClassId i
                            vi -> vi


elemOfAtom :: Foldable lang => Var -> Atom lang -> Bool
elemOfAtom x (Atom v l) = Var x == v || Var x `elem` toList l

-- | Given a database and a list of Atoms with an occurring var @x@, find
-- @D_x@, the domain of variable x, that is, the values x can take
intersectAtoms :: (Ord (lang ()), Foldable lang, Functor lang) => Database lang -> [Atom lang] -> [ClassId]
intersectAtoms (DB m) atoms = foldr1 intersect (map (\(Atom v l) -> intersectInTrie (relation l) (v:toList l)) atoms)
-- intersectAtoms (DB m) (Atom (ClassId i) l@(toList -> x:xs):atoms) = out (out (m M.! void l) IM.! i)
    where 
        relation l = m M.! void l

        intersectInTrie :: Fix ClassIdMap -> [ClassIdOrVar] -> [ClassId]
        intersectInTrie (In m) [] = error "intersectInRelation should always be called in 'queries' that have at least one var... something went wrong"
        intersectInTrie (In m) (Var x:xs) = map fst $ IM.toList m -- Return all possible values of var
        intersectInTrie (In m) (ClassId x:xs) = intersectInTrie (m IM.! x) xs -- Recurse after descending one layer of the trie map

