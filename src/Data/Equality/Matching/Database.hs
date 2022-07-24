{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
module Data.Equality.Matching.Database where

import Data.Maybe (mapMaybe)
import Control.Monad

import Data.Foldable (toList, foldl')
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set as S

-- import GHC.Data.TrieMap

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes
import Data.Equality.Language
import Data.Equality.Utils

-- | Query variable
type Var = Int

-- TODO: Should be M.Map ...
type Subst = [(Var, ClassId)]

data ClassIdOrVar = ClassId {-# UNPACK #-} !ClassId
                  | Var     {-# UNPACK #-} !Var
    deriving (Show, Eq, Ord)

toVar :: ClassIdOrVar -> Maybe Var
toVar (Var v) = Just v
toVar (ClassId _) = Nothing
{-# INLINE toVar #-}

-- | An Atom ... in pattern ... is R_f(v, v1, ..., vk), so we define it as a
-- functor ast over pattern variables + the additional var for the e-class id
data Atom lang
    = Atom !ClassIdOrVar !(lang ClassIdOrVar)

data Query lang
    = Query ![Var] ![Atom lang]
    | SelectAllQuery {-# UNPACK #-} !Var

-- | Database made of trie maps for each relation. Each relation is uniquely
-- identified by the expressions modulo children expressions (hence @lang ()@)
newtype Database l
    = DB (M.Map (Operator l) (Fix ClassIdMap))

-- instance Show (lang ()) => Show (Database lang) where
--     show (DB m) = unlines $ map (\(a,b) -> show a <> ": " <> show' 0 b) $ M.toList m where
--         show' :: Int -> Fix ClassIdMap -> String
--         show' s m = flip foldFix m $ \case
--             (IM.toList -> m') -> unlines $ map (\(k,w) -> show k <> " --> " <> w) m'

varsInQuery :: Foldable lang => Query lang -> [Var]
varsInQuery (SelectAllQuery x) = [x]
varsInQuery (Query _ atoms) = ordNub $ foldl' f mempty atoms
    where
        f :: Foldable lang => [Var] -> Atom lang -> [Var]
        f s = \case
            Atom (Var v) (toList -> l) -> (v : (mapMaybe toVar l)) <> s
            Atom _       (toList -> l) -> (mapMaybe toVar l) <> s
{-# INLINE varsInQuery #-} 

queryHeadVars :: Foldable lang => Query lang -> [Var]
queryHeadVars (SelectAllQuery x) = [x]
queryHeadVars (Query qv _) = qv
{-# INLINE queryHeadVars #-}

-- ROMES:TODO no longer so sure why the query needs [Var] (the free variables in
-- the query, since the substitution includes ALL variables)... again, weird

-- | Take a query and produce valid substitutions from query variables to actual
-- classids
--
-- ROMES:TODO a less ad-hoc/specialized implementation of generic join...
genericJoin :: Language l => Database l -> Query l -> [Subst]
-- We want to match against ANYTHING, so we return a valid substitution for
-- all existing e-class: get all relations and make a substition for each class in that relation, then join all substitutions across all classes
genericJoin (DB m) (SelectAllQuery x) = concatMap (\(_,Fix clss) -> map ((:[]) . (x,)) $ IM.keys clss) (M.toList m)
-- This is the last variable, so we return a valid substitution for every
-- possible value for the variable (hence, we prepend @x@ to each and make it
-- its own substitution)
genericJoin d q@(Query qv atoms) = case varsInQuery q of
    [] -> error "Query should always have at least one var"
    [x] -> map ((:[]) . (x,)) (domainX x)

    -- ROMES:TODO the reverse order is probably faster bc people tend to put
    -- variables on the left of constants? e.g. (x + 0 = x)
    x:_ -> concatMap
        (\x_in_D ->
            -- Each valid sub-query assumed the x -> x_in_D substitution
            map ((x,x_in_D):) $ genericJoin d (Query qv (substitute x x_in_D atoms)))
        (domainX x)
   where
       atomsWithX x = filter (x `elemOfAtom`) atoms
       domainX x = intersectAtoms x d (atomsWithX x)
{-# INLINABLE genericJoin #-}
{-# SCC genericJoin #-}

substitute :: Functor lang => Var -> ClassId -> [Atom lang] -> [Atom lang]
substitute r i = map $ \case
   Atom (Var v) l
     | v == r -> Atom (ClassId i) l
   Atom x l -> Atom x $ flip fmap l $ \case
                   Var vi
                     | vi == r -> ClassId i
                   vi -> vi
{-# INLINE substitute #-}

elemOfAtom :: Foldable lang => Var -> Atom lang -> Bool
elemOfAtom x (Atom v l) = Var x == v || Var x `elem` toList l
{-# INLINE elemOfAtom #-}


-- ROMES:TODO Terrible name 'intersectAtoms'

-- | Given a database and a list of Atoms with an occurring var @x@, find
-- @D_x@, the domain of variable x, that is, the values x can take
--
-- Returns the intset (class id set) of classes forming the domain of var @x@
intersectAtoms :: forall l. Language l => Var -> Database l -> [Atom l] -> [ClassId]
-- lookup ... >>= ... to make sure non-existing patterns don't crash
intersectAtoms var (DB db) (a:atoms) = S.toList $ foldr (\x xs -> (f x) `S.intersection` xs) (f a) atoms
  where
    -- Get the matching ids for an atom
    f :: Atom l -> S.Set ClassId
    f (Atom v l) = case M.lookup (Operator $ void l) db of

        -- If needed relation doesn't exist altogether, return the matching
        -- class ids (none). When intersecting, nothing will be available -- as expected
        Nothing -> mempty

        -- If needed relation does exist, find intersection in it
        -- Add list of found intersections to existing
        Just r  -> case intersectInTrie var mempty r (v:toList l) of
                     Nothing ->  error "intersectInTrie shouldn't return nothing out side of the recursion; failure here is denoted by an empty list of matching classes ... write better errror"
                     Just xs -> S.fromList $ xs

intersectAtoms _ _ [] = error "can't intersect empty list of atoms?"
{-# INLINABLE intersectAtoms #-}
{-# SCC intersectAtoms #-}

-- | Find the matching ids that a variable can take given a list of variables
-- and ids that must match the structure
--
-- Invalid substitutions are represented as Nothing
intersectInTrie :: Var -- ^ The variable whose domain we are looking for
                -> IM.IntMap ClassId -- ^ A mapping from variables that have been substituted
                -> Fix ClassIdMap -- ^ The trie
                -> [ClassIdOrVar]  -- ^ The "query"
                -> Maybe [ClassId] -- ^ The resulting domain for a valid substitution
intersectInTrie !var !substs (Fix !m) = \case

    [] -> error "intersectInRelation should always be called in 'queries' that have at least one var... something went wrong"

    [Var x] -> {-# SCC "SingleVar" #-} case IM.lookup x substs of
                 -- Last variable is a var, so all possible values are possible
                 Nothing -> pure (IM.keys m)
                 -- Last variable is a var but has a substitution, do the same as if it were the last ClassId
                 Just s -> IM.lookup s m >> pure [s]

    -- Last variable is constant (lol), so the valid intersection value is itself if it exists in the map
    [ClassId x] -> {-# SCC "SingleClassId" #-} IM.lookup x m >> pure [x]

    -- For all possible values of the required var, see which result in a non-empty
    -- intersection, in which its own occurence is replaced by the assumed value
    (Var x:xs)
      | x == var -> {-# SCC "THE_var_x:xs" #-} pure $
       mapMaybe
          (\(k, ls) -> do
            -- If the resulting intersection for this value of var @x@ is
            -- non-empty then it's a valid substitution. Otherwise, this
            -- variable doesn't match any the database and we return the empty
            -- list. If the intersection was successful we return the value for
            -- the var
            intersectInTrie var (putSubst x k substs) ls xs >> pure k) (IM.toList m)

      -- We found a var which isn't the target, so we'll assume all
      -- possible values of this variable and get intersections with the
      -- actual var after
      | otherwise -> {-# SCC "NOT_THE_var_x:xs" #-} case IM.lookup x substs of
          Nothing -> pure $ concat $ 
            mapMaybe
              (\(k, ls) ->
                -- The resulting intersection for this value of var @x@ is some
                -- of the possible values of the target var; return the sum of all valid
                intersectInTrie var (putSubst x k substs) ls xs) (IM.toList m)

          -- Recurse after descending one layer of the trie map using the substitution we have ffor this var
          Just varVal -> intersectInTrie var substs (m IM.! varVal) xs

    -- Recurse after descending one layer of the trie map
    (ClassId x:(xs)) -> {-# SCC "ClassId_x:xs" #-} intersectInTrie var substs (m IM.! x) xs
{-# INLINABLE intersectInTrie #-}
-- {-# SCC intersectInTrie #-}

putSubst :: Var -> ClassId -> IM.IntMap ClassId -> IM.IntMap ClassId
-- putSubst v i = M.update (\case Nothing -> Just i; Just _ -> error "replacing a subst!!") v
putSubst = IM.insert
{-# INLINE putSubst #-}

