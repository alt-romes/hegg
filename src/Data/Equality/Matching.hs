{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Equality.Matching
    ( module Data.Equality.Matching
    , module Data.Equality.Matching.Pattern
    , Database, Subst, Var
    )
    where

import Data.Maybe (mapMaybe)
import Data.Foldable (toList)

import Control.Monad
import Control.Monad.State

import qualified Data.Map    as M
import qualified Data.IntMap as IM

import Data.Equality.Utils
import Data.Equality.Graph
import Data.Equality.Matching.Database
import Data.Equality.Matching.Pattern

-- |  Matching a pattern on an e-graph returns substitutions for every variable
-- in the pattern and the e-class that matched the pattern
data Match = Match
    { matchSubst :: Subst
    , matchClassId :: {-# UNPACK #-} !ClassId
    }

-- | Match a pattern against an AST, returnin a list of equivalence classes
-- that match the pattern and the corresponding substitution
--
-- ematch takes the built database because it can be shared accross matching.
-- One can convert an e-graph to a database with 'eGraphToDatabase'
--
-- TODO: Perhaps e-graph could carry database and rebuild it on rebuild
ematch :: Language l
       => Database l
       -> Pattern l
       -> [Match]
ematch db patr =
    let (q, root) = compileToQuery patr
     in mapMaybe (f root) (genericJoin db q)
    where
        -- | Convert each substitution into a match by getting the class-id
        -- where we matched from the subst
        --
        -- If the substitution is empty there is no match
        f :: Var -> Subst -> Maybe Match
        f root s = if IM.null s then Nothing
                                else case IM.lookup root s of
                                       Nothing -> error "how is root not in map?"
                                       Just found -> pure (Match s found)

-- | Convert an e-graph into a database in which we do the conjunctive queries
--
-- @
-- newtype Database lang = DB (Map (lang ()) (Fix ClassIdMap))
--
-- data EGraph s = EGraph
--     { ...
--     , memo      :: Map (ENode s) ClassId
--     }
-- @
eGraphToDatabase :: Language l => EGraph l -> Database l
eGraphToDatabase eg@EGraph{..} = M.foldrWithKey (addENodeToDB eg) (DB mempty) memo
  where

    -- Add an enode in an e-graph, given its class, to a database
    addENodeToDB :: Language l => EGraph l -> ENode l -> ClassId -> Database l -> Database l
    addENodeToDB _ enode classid (DB m) =
        -- ROMES:TODO map find
        -- Insert or create a relation R_f(i1,i2,...,in) for lang in which 
        DB $ M.alter (populate (classid:children enode)) (operator enode) m

    -- Populate or create a triemap given the population D_x (ClassIds)
    populate :: [ClassId] -> Maybe (Fix ClassIdMap) -> Maybe (Fix ClassIdMap)
    populate ids Nothing = Just $ populate' ids (Fix mempty)
    populate ids (Just f) = Just $ populate' ids f

    -- Populate a triemap given the population D_x (ClassIds)
    populate' :: [ClassId] -> Fix ClassIdMap -> Fix ClassIdMap
    populate' [] (Fix m) = Fix m
    populate' (x:xs) (Fix m) = Fix $ IM.alter (alterPopulation xs) x m
      where
        -- Insert remaining ids population doesn't exist, recursively merge tries with remaining ids
        alterPopulation :: [ClassId] -> Maybe (Fix ClassIdMap) -> Maybe (Fix ClassIdMap)
        -- If trie map entry doesn't exist yet, populate an empty map with the remaining ids
        alterPopulation ids Nothing = Just $ populate' ids (Fix mempty)
        -- If trie map entry already exists, populate the existing map with the remaining ids
        alterPopulation ids (Just f) = Just $ populate' ids f
{-# SCC eGraphToDatabase #-}


data AuxResult lang = {-# UNPACK #-} !Var :~ [Atom lang]

-- Return distinct variables in a pattern
vars :: Foldable lang => Pattern lang -> [Var]
vars (VariablePattern x) = [x]
vars (NonVariablePattern p) = ordNub $ join $ map vars $ toList p

-- | Compiles a 'Pattern' to a 'Query' and returns the query root variable with
-- it.
-- The root variable's substitutions are the e-classes where the pattern
-- matched
compileToQuery :: (Traversable lang) => Pattern lang -> (Query lang, Var)
compileToQuery = flip evalState 0 . compile_to_query'
    where
        compile_to_query' :: (Traversable lang) => Pattern lang -> State Int (Query lang, Var)
        compile_to_query' (VariablePattern x) = return (SelectAllQuery x, x)
        compile_to_query' p@(NonVariablePattern _) = do
            root :~ atoms <- aux p
            return (Query (ordNub $ root:vars p) atoms, root)

        aux :: (Traversable lang) => Pattern lang -> State Int (AuxResult lang)
        aux (VariablePattern x) = return $ x :~ [] -- from definition in relational e-matching paper (needed for as base case for recursion)
        aux (NonVariablePattern p) = do
            v <- next
            auxs <- sequence (toList (fmap aux p))
            let boundVars = map (\(b :~ _) -> b) auxs
                atoms     = join $ map (\(_ :~ a) -> a) auxs
                -- Number of bound vars should match number of children of this
                -- lang. We can traverse the pattern and replace sub-patterns with
                -- their corresponding bound variable
                p' = evalState (subPatsToVars p boundVars) 0
            return (v :~ (Atom (Var v) (fmap Var p'):atoms))
                where
                    -- State keeps track of the index of the variable we're
                    -- taking from the bound vars array
                    subPatsToVars :: Traversable lang => lang (Pattern lang) -> [Var] -> State Int (lang Var)
                    subPatsToVars p' boundVars = traverse (const $ (boundVars !!) <$> next) p'
{-# SCC compileToQuery #-}

fresh :: State Int String
fresh = ('$':) . ('~':) . (letters !!) <$> next
    where letters :: [String]
          letters = [1..] >>= flip replicateM ['a'..'z']

-- Gives next negative int for bound variable
next :: State Int Int
next = do
    i <- get
    put (i+1)
    return i
