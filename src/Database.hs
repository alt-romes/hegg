{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database where

import Debug.Trace
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (intersect, union)
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

type Subst = [(Var, ClassId)]

data ClassIdOrVar = ClassId ClassId | Var Var
    deriving (Show, Eq)

toVar :: ClassIdOrVar -> Maybe Var
toVar (Var v) = Just v
toVar (ClassId _) = Nothing

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

varsInQuery :: Foldable lang => Query lang -> [Var]
varsInQuery (Query _ atoms) =
    case map (\case Atom (Var v) (toList -> l) -> v:mapMaybe toVar l; Atom _ (toList -> l) -> mapMaybe toVar l) atoms of
        [] -> []
        xs -> foldr1 union xs

-- ROMES:TODO no longer so sure why the query needs [Var] (the free variables in
-- the query, since the substitution includes ALL variables)... again, weird

-- | Take a query and produce valid substitutions from query variables to actual
-- classids
genericJoin :: (Ord (lang ()), Foldable lang, Functor lang) => Database lang -> Query lang -> [Subst]
genericJoin _ (varsInQuery -> []) = error "How did we get here?"
-- This is the last variable, so we return a valid substitution for every
-- possible value for the variable (hence, we prepend @x@ to each and make it
-- its own substitution)
genericJoin d q@(Query _ atoms)
  | [x] <- varsInQuery q
  = map ((:[]) . (x,)) (domainX x)
    where
        atomsWithX x = filter (x `elemOfAtom`) atoms
        domainX x = intersectAtoms d (atomsWithX x)
-- ROMES:TODO the reverse order is probably faster bc people tend to put
-- variables on the left of constants? e.g. (x + 0 = x)
genericJoin d q@(Query qv atoms)
  | x:xs <- varsInQuery q
  = flip concatMap (domainX x) $ \x_in_D ->
        -- Each valid sub-query assumed the x -> x_in_D substitution
        map ((x,x_in_D):) $ genericJoin d (Query qv (substitute x x_in_D atoms))
    where
        atomsWithX x = filter (x `elemOfAtom`) atoms
        domainX x = intersectAtoms d (atomsWithX x)

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

-- ROMES:TODO Terrible name
-- | Given a database and a list of Atoms with an occurring var @x@, find
-- @D_x@, the domain of variable x, that is, the values x can take
intersectAtoms :: (Ord (lang ()), Foldable lang, Functor lang) => Database lang -> [Atom lang] -> [ClassId]
-- lookup ... >>= ... to make sure non-existing patterns don't crash
intersectAtoms (DB m) atoms =
    case mapMaybe (\(Atom v l) -> M.lookup (void l) m >>= \r -> pure $ intersectInTrie r (v:toList l)) atoms of
      [] -> []
      ls -> foldr1 intersect ls

    where 
        relation l = m M.! void l

        intersectInTrie :: Fix ClassIdMap -> [ClassIdOrVar] -> [ClassId]
        intersectInTrie (In m) [] = error "intersectInRelation should always be called in 'queries' that have at least one var... something went wrong"
        intersectInTrie (In m) (Var x:xs) = map fst $ IM.toList m -- Return all possible values of var
        intersectInTrie (In m) (ClassId x:xs) = intersectInTrie (m IM.! x) xs -- Recurse after descending one layer of the trie map

