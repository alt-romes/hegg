{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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

-- | Take a query and produce a list of valid substitutions from query
-- variables to actual classids. Each list is a fully valid substitution on its
-- own
--
-- ROMES:TODO a less ad-hoc/specialized implementation of generic join...
-- ROMES:TODO query ordering is very important!
genericJoin :: Language l => Database l -> Query l -> [Subst]

-- We want to match against ANYTHING, so we return a valid substitution for
-- all existing e-class: get all relations and make a substition for each class in that relation, then join all substitutions across all classes
genericJoin (DB m) (SelectAllQuery x) = concatMap (map (IM.singleton x) . IM.keys . unFix) (M.elems m)

-- This is the last variable, so we return a valid substitution for every
-- possible value for the variable (hence, we prepend @x@ to each and make it
-- its own substitution)
genericJoin d q@(Query qv atoms) = case varsInQuery q of

    [] -> error "Query should always have at least one var"

    [x] -> map (IM.singleton x) (domainX x)

    x:_ -> concatMap
        (\x_in_D ->
            map (IM.alter (\case
                Nothing -> Just x_in_D
                Just _ -> error "overriding existing subst") x) $
                    -- Each valid sub-query assumed the x -> x_in_D substitution
                    genericJoin d (Query qv (substitute x x_in_D atoms)))
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
-- Returns the class id set of classes forming the domain of var @x@
intersectAtoms :: forall l. Language l => Var -> Database l -> [Atom l] -> [ClassId]
intersectAtoms !var (DB !db) (a:atoms) = S.toList $ foldr (\x xs -> (f x) `S.intersection` xs) (f a) atoms
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
                     Nothing ->  error "intersectInTrie should return valid substitution for variable query"
                     Just xs -> S.fromList $ xs

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
-- TODO: write a note for this...
--
--
-- TODO: Really, a valid substitution is one which isn't empty...
intersectInTrie :: Var -- ^ The variable whose domain we are looking for
                -> IM.IntMap ClassId -- ^ A mapping from variables that have been substituted
                -> Fix ClassIdMap  -- ^ The trie
                -> [ClassIdOrVar]  -- ^ The "query"
                -> Maybe [ClassId] -- ^ The resulting domain for a valid substitution
intersectInTrie !var !substs (Fix !m) = \case

    [] -> pure []

    -- Looking for a class-id, so if it exists in map the intersection is
    -- valid and we simply continue the search for the domain
    ClassId x:xs -> {-# SCC "Intersect_ClassId" #-}
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
        Just varVal -> {-# SCC "intersect_subst_var" #-} IM.lookup varVal m >>= \next -> intersectInTrie var substs next xs
        -- (1) or (3)
        Nothing -> {-# SCC "intersect_new_var" #-}
           pure $ IM.foldrWithKey (\(!k) (!ls) (!acc) -> 
            case intersectInTrie var (putSubst x k substs) ls xs of
                Nothing -> acc
                Just rs -> 
                  if x == var
                      -- (1)
                      then k:acc
                      -- (3)
                      else rs <> acc) mempty m

--            if x == var
--               -- (1)
--               then 
--                   -- If the resulting intersection for this value of var @x@ is
--                   -- valid then it's a valid substitution. Otherwise, this
--                   -- variable doesn't match any the database and we don't add
--                   -- this value to the domain. If the intersection was
--                   -- successful we return the value for the var.
--                   -- 
--                   -- NOTE: Using empty lists instead of Maybe to detect
--                   -- failure is not feasable, because intersections with e.g.
--                   -- class ids only result in empty lists, but might be valid
--                   -- or invalid depending on whether we got a match or not.
--                   -- That's why we use Maybe to identify failed and successful
--                   -- matches
--                     Nothing -> acc
--                     Just _  -> k:acc

--               -- (3)
--               else {-# SCC "intersect_new_SOME_var" #-}
--                    -- The resulting intersection for this value of var @x@ is some
--                    -- of the possible values of the target var; return the sum of all valid
--                    case intersectInTrie var (putSubst x k substs) ls xs of
--                      Just rs -> rs <> acc)
--                      mempty m

{-# INLINABLE intersectInTrie #-}
-- {-# SCC intersectInTrie #-}

putSubst :: Var -> ClassId -> IM.IntMap ClassId -> IM.IntMap ClassId
-- putSubst v i = M.update (\case Nothing -> Just i; Just _ -> error "replacing a subst!!") v
putSubst = IM.insert
{-# SCC putSubst #-}

