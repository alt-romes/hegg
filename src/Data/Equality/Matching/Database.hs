{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Data.Equality.Matching.Database where

import Data.List (sortBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Control.Monad

import Data.Foldable as F (toList, foldl', length)
import qualified Data.Map.Strict    as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Data.Equality.Graph.Classes.Id
import Data.Equality.Graph.Nodes
import Data.Equality.Language

-- | Query variable
type Var = Int

-- | Mapping from 'Var' to @['ClassId']@. Multiple possible substitutions for
-- each variable
type Subst = IM.IntMap ClassId

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

atomLength :: Foldable lang => Atom lang -> Int
atomLength (Atom _ l) = 1 + F.length l
{-# SCC atomLength #-}

data Query lang
    = Query ![Var] ![Atom lang]
    | SelectAllQuery {-# UNPACK #-} !Var

-- | Database made of trie maps for each relation. Each relation is uniquely
-- identified by the expressions modulo children expressions (hence @lang ()@)
newtype Database l
    = DB (M.Map (Operator l) IntTrie)

-- | A triemap that keeps a cache of all keys in each level
data IntTrie = MkIntTrie
  { tkeys :: IS.IntSet
  , trie :: IM.IntMap IntTrie
  }

-- instance Show (lang ()) => Show (Database lang) where
--     show (DB m) = unlines $ map (\(a,b) -> show a <> ": " <> show' 0 b) $ M.toList m where
--         show' :: Int -> Fix ClassIdMap -> String
--         show' s m = flip foldFix m $ \case
--             (IM.toList -> m') -> unlines $ map (\(k,w) -> show k <> " --> " <> w) m'

-- ROMES:TODO: Batching? How? https://arxiv.org/pdf/2108.02290.pdf
orderedVarsInQuery :: Foldable lang => Query lang -> [Var]
orderedVarsInQuery (SelectAllQuery x) = [x]
orderedVarsInQuery (Query _ atoms) = IS.toList . IS.fromAscList $ sortBy (compare `on` varCost) $ mapMaybe toVar $ foldl' f mempty atoms
    where
        f :: Foldable lang => [ClassIdOrVar] -> Atom lang -> [ClassIdOrVar]
        f s (Atom v (toList -> l)) = v:(l <> s)
        {-# SCC f #-}

        -- First, prioritize variables that occur in many relations; second,
        -- prioritize variables that occur in small relations
        varCost :: Var -> Int
        varCost v = foldr (\a acc -> if v `elemOfAtom` a then acc - 100 + atomLength a else acc) 0 atoms
        {-# SCC varCost #-}
{-# SCC orderedVarsInQuery #-} 

queryHeadVars :: Foldable lang => Query lang -> [Var]
queryHeadVars (SelectAllQuery x) = [x]
queryHeadVars (Query qv _) = qv
{-# INLINE queryHeadVars #-}

-- | Take a query and produce a list of valid substitutions from query
-- variables to actual classids. Each list is a fully valid substitution on its
-- own
--
-- ROMES:TODO a less ad-hoc/specialized implementation of generic join...
-- ROMES:TODO query ordering is very important!
genericJoin :: forall l. Language l => Database l -> Query l -> [Subst]

-- We want to match against ANYTHING, so we return a valid substitution for
-- all existing e-class: get all relations and make a substition for each class in that relation, then join all substitutions across all classes
genericJoin (DB m) (SelectAllQuery x) = concatMap (map (IM.singleton x) . IS.toList . tkeys) (M.elems m)

-- This is the last variable, so we return a valid substitution for every
-- possible value for the variable (hence, we prepend @x@ to each and make it
-- its own substitution)
-- ROMES:TODO: Start here. Map vars to indexs in an array and substitute in the resulting subst
genericJoin d q@(Query _ atoms) = genericJoin' atoms (orderedVarsInQuery q)

 where
   genericJoin' :: [Atom l] -> [Var] -> [Subst]
   genericJoin' atoms' = \case

     [] -> map mempty atoms

     x:xs -> IS.foldl'
         (\acc x_in_D ->
           map (IM.insert x x_in_D)
             -- Each valid sub-query assumed the x -> x_in_D substitution
             (genericJoin' (substitute x x_in_D atoms') xs)
               <> acc)
         mempty
         (domainX x atoms')
   {-# SCC genericJoin' #-}

   atomsWithX :: Var -> [Atom l] -> [Atom l]
   atomsWithX x = filter (x `elemOfAtom`)
   {-# INLINE atomsWithX #-}

   domainX :: Var -> [Atom l] -> IS.IntSet
   domainX x = intersectAtoms x d . atomsWithX x
   {-# INLINE domainX #-}

{-# INLINABLE genericJoin #-}
{-# SCC genericJoin #-}

substitute :: Functor lang => Var -> ClassId -> [Atom lang] -> [Atom lang]
substitute !r !i = map $ \case
   Atom (Var v) l
     | v == r -> Atom (ClassId i) l
   Atom x l -> Atom x $ flip fmap l $ \case
                   Var vi
                     | vi == r -> ClassId i
                   vi -> vi
{-# SCC substitute #-}

elemOfAtom :: Foldable lang => Var -> Atom lang -> Bool
elemOfAtom x (Atom v l) = case v of
  Var v' -> x == v'
  _ -> foldr (\i acc -> case i of Var v' -> x == v' || acc; _ -> acc) False (toList l)
{-# SCC elemOfAtom #-}


-- ROMES:TODO Terrible name 'intersectAtoms'

-- | Given a database and a list of Atoms with an occurring var @x@, find
-- @D_x@, the domain of variable x, that is, the values x can take
--
-- Returns the class id set of classes forming the domain of var @x@
intersectAtoms :: forall l. Language l => Var -> Database l -> [Atom l] -> IS.IntSet
intersectAtoms !var (DB db) (a:atoms) = foldr (\x xs -> (f x) `IS.intersection` xs) (f a) atoms
  where
    -- Get the matching ids for an atom
    f :: Atom l -> IS.IntSet
    f (Atom v l) = case M.lookup (Operator $ void l) db of

        -- If needed relation doesn't exist altogether, return the matching
        -- class ids (none). When intersecting, nothing will be available -- as expected
        Nothing -> mempty

        -- If needed relation does exist, find intersection in it
        -- Add list of found intersections to existing
        Just r  -> case intersectInTrie var mempty r (v:toList l) of
                     Nothing ->  error "intersectInTrie should return valid substitution for variable query"
                     Just xs -> xs

intersectAtoms _ _ [] = error "can't intersect empty list of atoms?"
{-# INLINABLE intersectAtoms #-}
{-# SCC intersectAtoms #-}

-- | Find the matching ids that a variable can take given a list of variables
-- and ids that must match the structure
--
-- Invalid substitutions are represented as Nothing
--
-- The intersection might be invalid while assuming values for variables. If
-- we're looking for the domain of some variables we should never get an
-- invalid substitution, but rather an empty list saying that the query
-- intersection is valid but empty.
--
--
-- If R_f(1,y,z), this function receives [1,y,z] :: [ClassIdOrVar] and
-- intersects the trie map of R_f with this prefix
--
-- TODO: write a note for this...
--
--
-- TODO: Really, a valid substitution is one which isn't empty...
intersectInTrie :: Var -- ^ The variable whose domain we are looking for
                -> IM.IntMap ClassId -- ^ A mapping from variables that have been substituted
                -> IntTrie  -- ^ The trie
                -> [ClassIdOrVar]  -- ^ The "query"
                -> Maybe IS.IntSet -- ^ The resulting domain for a valid substitution
intersectInTrie !var !substs (MkIntTrie trieKeys m) = \case

    [] -> pure []

    -- Looking for a class-id, so if it exists in map the intersection is
    -- valid and we simply continue the search for the domain
    ClassId x:xs ->
        IM.lookup x m >>= \next -> intersectInTrie var substs next xs

    -- Looking for a var. It might be one of the following:
    --
    --      (1) The variable whose domain we're looking for, and this is the
    --      first time we found it. In this case we'll assume all substitutions
    --      are valid, and try to get a valid substitution with that
    --      assumption. If the substitution is valid, the substitution is an
    --      element of the domain.
    --
    --      (2) The variable whose domain we're looking for, but we've already
    --      assumed a value for it in this branch, so we continue the recursion
    --      guaranteeing the assumption results in a valid substitution
    --
    --      (3) A bound variable, and this is the first time we find it. We
    --      assume its value for all branches and concatenate the result of all
    --      valid domain elements for each branch that resulted in a valid
    --      substitution
    --
    --      (4) A bound variable, but we've assumed a value for it, so we
    --      continue the recursion again to validate the assumption and
    --      possibly find the domain of the variable we're looking for ahead
    --
    Var x:xs -> case IM.lookup x substs of
        -- (2) or (4), we simply continue
        Just varVal -> IM.lookup varVal m >>= \next -> intersectInTrie var substs next xs
        -- (1) or (3)
        Nothing -> pure $ if x == var
          -- (1)
          then
            -- If this is the var we're looking for, and the remaining @xs@
            -- suffix only consists of variables modulo the var we're looking
            -- for, we can simply return all possible keys for this since it is
            -- the correct variable. This is quite important for performance!
            if all (isVarDifferentFrom x) xs
              then trieKeys
              else IM.foldrWithKey (\k ls (!acc) ->
               case intersectInTrie var (IM.insert x k substs) ls xs of
                   Nothing -> acc
                   Just _  -> k `IS.insert` acc
                         ) mempty m
          -- (3)
          -- else {-# SCC "intersect_new_OTHER_var" #-} IS.unions $ IM.elems $ IM.mapMaybeWithKey (\k ls -> intersectInTrie var ({-# SCC "putSubst" #-} IM.insert x k substs) ls xs) m
          else IM.foldrWithKey (\k ls (!acc) ->
            case intersectInTrie var (IM.insert x k substs) ls xs of
                Nothing -> acc
                Just rs -> rs <> acc) mempty m


{-# INLINABLE intersectInTrie #-}
{-# SCC intersectInTrie #-}

isVarDifferentFrom :: Var -> ClassIdOrVar -> Bool
isVarDifferentFrom _ (ClassId _) = False
isVarDifferentFrom x (Var     y) = x /= y
{-# INLINE isVarDifferentFrom #-}
