{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Equality.Matching
    ( module Data.Equality.Matching
    , Subst
    )
    where

import Data.String
import Data.Maybe

import Data.List (nub)
import Data.Foldable (toList)

import Data.Fix

import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S

import Data.Equality.Graph

import Database

-- | EGS version of 'ematch'
ematchM :: (Ord (lang ()), Traversable lang)
       => PatternAST lang
       -> EGS lang [(Subst, ClassId)]
ematchM pat = gets (ematch pat)

-- | Match a pattern against an AST, returnin a list of equivalence classes
-- that match the pattern and the corresponding substitution
ematch :: (Ord (lang ()), Traversable lang)
       => PatternAST lang
       -> EGraph lang
       -> [(Subst, ClassId)]
ematch pat eg =
    let db = eGraphToDatabase eg
        q = compileToQuery pat
    -- genericJoin folds all matches, so, starting with the root variable, we group a level of the query by the root variable to get the substitutions for each e-class
     in mapMaybe floatOutEClass (genericJoin db q)
    where
        -- Given a substitution in which the first element is the pair
        -- (root_var,root_class), float the root_class out and return it with
        -- the substitution
        floatOutEClass [] = Nothing
        floatOutEClass ((_,root_class):xs) = Just (xs, root_class)

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
eGraphToDatabase :: (Ord (lang ()), Functor lang, Foldable lang) => EGraph lang -> Database lang
eGraphToDatabase eg@(EGraph {..}) = M.foldrWithKey (addENodeToDB eg) (DB M.empty) memo
  where

    -- Add an enode in an e-graph, given its class, to a database
    addENodeToDB :: (Ord (lang ()), Functor lang, Foldable lang) => EGraph lang -> ENode lang -> ClassId -> Database lang -> Database lang
    addENodeToDB eg enode classid (DB m) =
        -- ROMES:TODO map find
        -- Insert or create a relation R_f(i1,i2,...,in) for lang in which 
        DB $ M.alter (populate (classid:toList enode)) (void enode) m

    -- Populate or create a triemap given the population D_x (ClassIds)
    populate :: [ClassId] -> Maybe (Fix ClassIdMap) -> Maybe (Fix ClassIdMap)
    populate ids Nothing = Just $ populate' ids (Fix IM.empty)
    populate ids (Just f) = Just $ populate' ids f

    -- Populate a triemap given the population D_x (ClassIds)
    populate' :: [ClassId] -> Fix ClassIdMap -> Fix ClassIdMap
    populate' [] (Fix m) = Fix m
    populate' (x:xs) (Fix m) = Fix $ IM.alter (alterPopulation xs) x m
      where
        -- Insert remaining ids population doesn't exist, recursively merge tries with remaining ids
        alterPopulation :: [ClassId] -> Maybe (Fix ClassIdMap) -> Maybe (Fix ClassIdMap)
        -- If trie map entry doesn't exist yet, populate an empty map with the remaining ids
        alterPopulation ids Nothing = Just $ populate' ids (Fix IM.empty)
        -- If trie map entry already exists, populate the existing map with the remaining ids
        alterPopulation ids (Just f) = Just $ populate' ids f



-- | @(~x + 0) --> BinOp Add (Var "~x") (ENode (Integer 0))@
-- @~x --> VariablePattern "~x"@
data PatternAST lang = NonVariablePattern (lang (PatternAST lang)) | VariablePattern Var
deriving instance Show (lang (PatternAST lang)) => Show (PatternAST lang)

instance IsString (PatternAST lang) where
    fromString = VariablePattern

data AuxResult lang = Var :~ [Atom lang]

vars :: Foldable lang => PatternAST lang -> [Var]
vars (VariablePattern x) = [x]
vars (NonVariablePattern p) = nub $ join $ map vars $ toList p

compileToQuery :: (Traversable lang) => PatternAST lang -> Query lang
compileToQuery = flip evalState 0 . compile_to_query'
    where
        compile_to_query' :: (Traversable lang) => PatternAST lang -> State Int (Query lang)
        compile_to_query' (VariablePattern _) = error "sole variable pattern doesn't generate any atoms"
        compile_to_query' p = do
            root :~ atoms <- aux p
            return (Query (S.fromList $ root:vars p) atoms)

        aux :: (Traversable lang) => PatternAST lang -> State Int (AuxResult lang)
        aux (VariablePattern x) = return (x :~ [])
        aux (NonVariablePattern p) = do
            v <- fresh
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
                    subPatsToVars :: Traversable lang => lang (PatternAST lang) -> [Var] -> State Int (lang Var)
                    subPatsToVars p boundVars = traverse (const $ (boundVars !!) <$> next) p

fresh :: State Int String
fresh = ('$':) . ('~':) . (letters !!) <$> next
    where letters :: [String]
          letters = [1..] >>= flip replicateM ['a'..'z']

next :: State Int Int
next = do
    i <- get
    put (i+1)
    return i

