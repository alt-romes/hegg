{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Database where

import Data.Fix

import Data.Maybe (mapMaybe)
import Data.List (intersect, union, nub)
import Control.Monad

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM

-- import GHC.Data.TrieMap

import Data.Equality.Graph

-- | Query variable
type Var = String

type Subst = [(Var, ClassId)]

data ClassIdOrVar = ClassId ClassId | Var Var
    deriving (Show, Eq, Ord)

toVar :: ClassIdOrVar -> Maybe Var
toVar (Var v) = Just v
toVar (ClassId _) = Nothing

-- | An Atom ... in pattern ... is R_f(v, v1, ..., vk), so we define it as a
-- functor ast over pattern variables + the additional var for the e-class id
data Atom lang
    = Atom ClassIdOrVar (lang ClassIdOrVar)

data Query lang
    = Query [Var] [Atom lang]
    | SelectAllQuery Var

-- | Database made of trie maps for each relation. Each relation is uniquely
-- identified by the expressions modulo children expressions (hence @lang ()@)
newtype Database l
    = DB (M.HashMap (Operator l) (Fix ClassIdMap))

-- instance Show (lang ()) => Show (Database lang) where
--     show (DB m) = unlines $ map (\(a,b) -> show a <> ": " <> show' 0 b) $ M.toList m where
--         show' :: Int -> Fix ClassIdMap -> String
--         show' s m = flip foldFix m $ \case
--             (IM.toList -> m') -> unlines $ map (\(k,w) -> show k <> " --> " <> w) m'

varsInQuery :: Foldable lang => Query lang -> [Var]
varsInQuery (SelectAllQuery x) = [x]
varsInQuery (Query _ atoms) =
    case flip map atoms $ \case
            Atom (Var v) (toList -> l) -> v:mapMaybe toVar l
            Atom _ (toList -> l) -> mapMaybe toVar l
    of
        [] -> []
        xs -> nub $ foldr1 union xs

queryHeadVars :: Foldable lang => Query lang -> [Var]
queryHeadVars (SelectAllQuery x) = [x]
queryHeadVars (Query qv _) = qv

-- ROMES:TODO no longer so sure why the query needs [Var] (the free variables in
-- the query, since the substitution includes ALL variables)... again, weird

-- | Take a query and produce valid substitutions from query variables to actual
-- classids
--
-- ROMES:TODO a less ad-hoc/specialized implementation of generic join...
genericJoin :: Language l => Database l -> Query l -> [Subst]
-- We want to match against ANYTHING, so we return a valid substitution for
-- all existing e-class: get all relations and make a substition for each class in that relation, then join all substitutions across all classes
genericJoin (DB m) (SelectAllQuery x) = concatMap (\(_,Fix clss) -> map ((:[]) . (x,) . fst) $ IM.toList clss) (M.toList m)
-- This is the last variable, so we return a valid substitution for every
-- possible value for the variable (hence, we prepend @x@ to each and make it
-- its own substitution)
genericJoin d q@(Query qv atoms)
  | [x] <- varsInQuery q
  = map ((:[]) . (x,)) (domainX x)
-- ROMES:TODO the reverse order is probably faster bc people tend to put
-- variables on the left of constants? e.g. (x + 0 = x)
  | x:_ <- varsInQuery q
  = flip concatMap (domainX x) $ \x_in_D ->
        -- Each valid sub-query assumed the x -> x_in_D substitution
        map ((x,x_in_D):) $ genericJoin d (Query qv (substitute x x_in_D atoms))
  | otherwise = error "Query should always have at least one var"
   where
       atomsWithX x = filter (x `elemOfAtom`) atoms
       domainX x = intersectAtoms x d (atomsWithX x)

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
intersectAtoms :: Language l => Var -> Database l -> [Atom l] -> [ClassId]
-- lookup ... >>= ... to make sure non-existing patterns don't crash
intersectAtoms var (DB db) atoms =
    case flip map atoms $ \(Atom v l) -> case M.lookup (Operator $ void l) db of
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
        -- Because ordering is left to right, first var observed is the
        -- variable we're targeting
        intersectInTrie :: Fix ClassIdMap -> [ClassIdOrVar] -> Maybe [ClassId]
        intersectInTrie (Fix _) [] = error "intersectInRelation should always be called in 'queries' that have at least one var... something went wrong"
        -- Last variable is a var, so all possible values are possible
        intersectInTrie (Fix m) [Var _] = Just (map fst $ IM.toList m)

        -- Last variable is constant (lol), so the valid intersection value is itself if it exists in the map
        intersectInTrie (Fix m) [ClassId x] = IM.lookup x m >> pure [x]

        -- ROMES:TODO: we assume that the first variable is the one we want, but that's not true.
        -- For all possible values of the required var, see which result in a non-empty
        -- intersection, in which its own occurence is replaced by the assumed value
        intersectInTrie (Fix m) (Var x:xs)
          | x == var = Just $
            flip mapMaybe (IM.toList m) $ \(k, ls) -> do
                -- The resulting intersection for this value of var @x@ is
                -- non-empty, meaning we can return it as a valid substitution
                _ <- intersectInTrie ls (subst x k xs)
                -- This will only happen if intersectInTrie returned @Just _@ (intersection was successful)
                pure k
          -- We found a var which isn't the target, so we'll assume all
          -- possible values of this variable and get intersections with the
          -- actual var after
          | otherwise = Just $
            -- ROMES:TODO do I need nub?
            nub $ concat $ flip mapMaybe (IM.toList m) $ \(k, ls) -> do
                -- The resulting intersection for this value of var @x@ is some
                -- of the possible values of the target var; return the sum of all valid
                intersectInTrie ls (subst x k xs)


        intersectInTrie (Fix m) (ClassId x:xs) = intersectInTrie (m IM.! x) xs -- Recurse after descending one layer of the trie map

        subst :: Var -> ClassId -> [ClassIdOrVar] -> [ClassIdOrVar]
        subst v i = map (\case Var v' | v == v' -> ClassId i; x -> x)

