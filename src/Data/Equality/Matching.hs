{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-|
   Equality-matching, implemented using a relational database
   (defined in 'Data.Equality.Matching.Database') according to the paper
   \"Relational E-Matching\" https://arxiv.org/abs/2108.02290.
 -}
module Data.Equality.Matching
    ( ematch
    , eGraphToDatabase
    , Match(..)

    -- * Compiling a Pattern to a Query
    , compileToQuery
    , VarsState(varNames), findVarName
    , userPatVars

    , module Data.Equality.Matching.Pattern
    )
    where

import Data.Maybe (mapMaybe)
import Data.Foldable (toList)
import Data.Containers.ListUtils

import Control.Monad
import Control.Monad.Trans.State.Strict

import qualified Data.Map.Strict    as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Data.Equality.Graph
import Data.Equality.Graph.Lens
import Data.Equality.Matching.Database
import Data.Equality.Matching.Pattern
import Data.Coerce (coerce)

-- | Matching a pattern on an e-graph returns the e-class in which the pattern
-- was matched and an e-class substitution for every 'VariablePattern' in the pattern.
data Match = Match
    { matchSubst :: !Subst
    , matchClassId :: {-# UNPACK #-} !ClassId
    }

-- TODO: Perhaps e-graph could carry database and rebuild it on rebuild

-- | Match a pattern 'Query', gotten from 'compileToQuery' on a 'Pattern',
-- against a 'Database', which is built from an 'EGraph' with 'eGraphToDatabase'.
--
-- Returns a list of matches, one 'Match' for each set of valid substitutions
-- for all variables and the equivalence class in which the pattern was matched.
--
-- 'ematch' takes a 'Database' instead of an 'EGraph' because the 'Database'
-- could be constructed only once and shared accross matching.
ematch :: Language l
       => Database l
       -> (Query l, Var {- query root var -})
       -> [Match]
ematch db (q, root) =
    let
        -- | Convert each substitution into a match by getting the class-id
        -- where we matched from the subst
        --
        -- If the substitution is empty there is no match
        f :: Subst -> Maybe Match
        f s | nullSubst s
            = Nothing
            | let found = findSubst root s
            = pure (Match s found)

     in mapMaybe f (genericJoin db q)

-- | Convert an e-graph into a database
eGraphToDatabase :: Language l => EGraph a l -> Database l
eGraphToDatabase egr = foldrWithKeyNM' addENodeToDB (DB mempty) (egr^._memo)
  where

    -- Add an enode in an e-graph, given its class, to a database
    addENodeToDB :: Language l => ENode l -> ClassId -> Database l -> Database l
    addENodeToDB enode classid (DB m) =
        -- ROMES:TODO map find
        -- Insert or create a relation R_f(i1,i2,...,in) for lang in which 
        DB $ M.alter (Just . populate (classid:children enode)) (operator enode) m

    -- Populate or create a triemap given the population D_x (ClassIds)
    -- Insert remaining ids population doesn't exist, recursively merge tries with remaining ids
    populate :: [ClassId] -> Maybe IntTrie -> IntTrie
    -- If trie map entry doesn't exist yet, populate an empty map with the remaining ids
    populate []     Nothing = MkIntTrie mempty mempty
    populate (x:xs) Nothing = MkIntTrie (IS.singleton x) $ IM.singleton x (populate xs Nothing)
    -- If trie map entry already exists, populate the existing map with the remaining ids
    populate []     (Just it)              = it
    populate (x:xs) (Just (MkIntTrie k m)) = MkIntTrie (x `IS.insert` k) $ IM.alter (Just . populate xs) x m
{-# INLINABLE eGraphToDatabase #-}


-- * Database related internals

-- | Auxiliary result in 'compileToQuery' algorithm
data AuxResult lang = {-# UNPACK #-} !Var :~ [Atom lang]


-- | Compiles a 'Pattern' to a 'Query' and returns the query root variable with
-- it.
-- The root variable's substitutions are the e-classes where the pattern
-- matched
compileToQuery :: (Traversable lang) => Pattern lang -> ((Query lang, Var), VarsState)
compileToQuery (VariablePattern n) = flip runState emptyVarsState $ do
  v <- getVarName n
  return (SelectAllQuery v, v)
compileToQuery pa =

  let (root :~ atoms, varsState) = runState (aux pa) emptyVarsState
   in ((Query (nubVars $ root:userPatVars varsState) atoms, root), varsState)

    where

        aux :: (Traversable lang) => Pattern lang -> State VarsState (AuxResult lang)
        aux (VariablePattern x) = do
          v <- getVarName x
          return (v :~ []) -- from definition in relational e-matching paper (needed for as base case for recursion)
        aux (NonVariablePattern p) = do
            v <- nextVar
            (toList -> auxs) <- traverse aux p
            let boundVars = map (\(b :~ _) -> b) auxs
                atoms     = join $ map (\(_ :~ a) -> a) auxs
                -- Number of bound vars should match number of children of this
                -- lang. We can traverse the pattern and replace sub-patterns with
                -- their corresponding bound variable
                p' = evalState (subPatsToVars p boundVars) 0
            return (v :~ (Atom (CVar v) (fmap CVar p'):atoms))
                where
                    -- State keeps track of the index of the variable we're
                    -- taking from the bound vars array
                    subPatsToVars :: Traversable lang => lang (Pattern lang) -> [Var] -> State Int (lang Var)
                    subPatsToVars p' boundVars = traverse (const $ (boundVars !!) <$> (get >>= \i -> modify' (+1) >> return i)) p'
{-# INLINABLE compileToQuery #-}

--------------------------------------------------------------------------------
-- ** Vars utils for compileToQuery
--------------------------------------------------------------------------------

-- | Map user-given Variable names to internal 'Var's plus a counter
data VarsState = VarsState
  { varNames  :: !(M.Map String Var)
  , nextVarId :: !Int
  }

-- | An empty 'VarsState'
emptyVarsState :: VarsState
emptyVarsState = VarsState mempty 0

-- | Compute the next internal 'Var' from the current 'VarNameMap'
nextVar :: State VarsState Var
nextVar = do
  n <- gets nextVarId
  modify' (\vs -> vs{nextVarId = nextVarId vs +1})
  return (MatchVar n)

-- | Add a name to the 'VarNameMap' and get the resulting 'Var'.
getVarName :: String -> State VarsState Var
getVarName s = do
  vm <- gets varNames
  case M.lookup s vm of
    Nothing -> do
      n <- nextVar
      modify' (\vs -> vs{varNames = M.insert s n (varNames vs)})
      return n
    Just v ->
      return v

findVarName :: VarsState -> String -> Var
findVarName vs s = (varNames vs) M.! s

-- | Return the variables given in a pattern by a user, from the 'VarsState'
userPatVars :: VarsState -> [Var]
userPatVars = M.elems . varNames

-- | Deduplicate list of pattern Vars
nubVars :: [Var] -> [Var]
nubVars = coerce . nubInt . coerce
