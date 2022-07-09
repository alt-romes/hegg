{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database where

-- ROMES:TODO use Data.Fix

import Debug.Trace
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (intersect, union, nub)
import Control.Monad

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Set as S

-- import GHC.Data.TrieMap

import Data.Equality.Graph

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

data Query lang = Query (S.Set Var) [Atom lang]
deriving instance (Show (lang ClassIdOrVar), Show (lang Var)) => Show (Query lang)

-- | Database made of trie maps for each relation. Each relation is uniquely
-- identified by the expressions modulo children expressions (hence @lang ()@)
newtype Database lang = DB (Map (lang ()) (Fix ClassIdMap))
deriving instance (Show (Fix ClassIdMap), Show (lang ())) => Show (Database lang)

varsInQuery :: Foldable lang => Query lang -> [Var]
varsInQuery (Query _ atoms) =
    case map (\case Atom (Var v) (toList -> l) -> v:mapMaybe toVar l; Atom _ (toList -> l) -> mapMaybe toVar l) atoms of
        [] -> []
        xs -> nub $ foldr1 union xs

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
    case flip map atoms $ \(Atom v l) -> case M.lookup (void l) m of
            -- If needed relation doesn't exist altogether, return the matching
            -- class ids (none). When intersecting, nothing will be available
            Nothing -> []
            -- If needed relation does exist, find intersection in it
            Just r  -> case intersectInTrie r (v:toList l) of
                         Nothing -> error "intersectInTrie shouldn't return nothing outside of the recursion; failure here is denoted by an empty list of matching classes"
                         Just rs -> rs
                         of
      [] -> []
      ls -> foldr1 intersect ls

    where 
        relation l = m M.! void l

        -- Because ordering is left to right, first var observed is the
        -- variable we're targeting
        intersectInTrie :: Fix ClassIdMap -> [ClassIdOrVar] -> Maybe [ClassId]
        intersectInTrie (In m) [] = error "intersectInRelation should always be called in 'queries' that have at least one var... something went wrong"
        -- Last variable is a var, so all possible values are possible
        intersectInTrie (In m) [Var x] = Just (map fst $ IM.toList m)

        -- Last variable is constant (lol), so the valid intersection value is itself if it exists in the map
        intersectInTrie (In m) [ClassId x] = IM.lookup x m >> pure [x]

        -- For all possible values of var see which result in a non-empty
        -- intersection, in which its own occurence is replaced by the assumed value
        intersectInTrie (In m) (Var x:xs) = Just $
            flip mapMaybe (IM.toList m) $ \(k, ls) -> do
                -- The resulting intersection for this value of var @x@ is
                -- non-empty, meaning we can return it as a valid substitution
                _ <- intersectInTrie ls (subst x k xs)
                -- This will only happen if intersectInTrie returned @Just _@ (intersection was successful)
                pure k


        intersectInTrie (In m) (ClassId x:xs) = intersectInTrie (m IM.! x) xs -- Recurse after descending one layer of the trie map

        subst :: Var -> ClassId -> [ClassIdOrVar] -> [ClassIdOrVar]
        subst v i = map (\case Var v' | v == v' -> ClassId i; x -> x)

