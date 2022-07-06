{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module EMatching where

import Data.String
import Data.Maybe

import Data.Foldable (toList)

import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.IntMap as IM

import EGraph.ENode
import EGraph.EClass
import EGraph

import Database

ematch :: (Ord (lang ()), Traversable lang)
       => PatternAST lang
       -> EGraph lang
       -> [(Var, ClassId)]
ematch pat eg =
    let db = eGraphToDatabase eg
        q  = compileToQuery pat
     in genericJoin db q
    

-- newtype Database lang = DB (Map (lang ()) (Fix ClassIdMap))
--
-- data EGraph s = EGraph
--     { ...
--     , memo      :: Map (ENode s) ClassId
--     }

-- | Convert an e-graph into a database in which we do the conjunctive queries
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
    populate ids Nothing = Just $ populate' ids (In IM.empty)
    populate ids (Just f) = Just $ populate' ids f

    -- Populate a triemap given the population D_x (ClassIds)
    populate' :: [ClassId] -> Fix ClassIdMap -> Fix ClassIdMap
    populate' [] (In m) = In m
    populate' (x:xs) (In m) = In $ IM.alter (alterPopulation xs) x m
      where
        -- Insert remaining ids population doesn't exist, recursively merge tries with remaining ids
        alterPopulation :: [ClassId] -> Maybe (Fix ClassIdMap) -> Maybe (Fix ClassIdMap)
        -- If trie map entry doesn't exist yet, populate an empty map with the remaining ids
        alterPopulation ids Nothing = Just $ populate' ids (In IM.empty)
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
vars (NonVariablePattern p) = join $ map vars $ toList p

compileToQuery :: (Traversable lang) => PatternAST lang -> Query lang
compileToQuery = flip evalState 0 . compile_to_query'
    where
        compile_to_query' :: (Traversable lang) => PatternAST lang -> State Int (Query lang)
        compile_to_query' p = do
            root :~ atoms <- aux p
            return (Query (root:vars p) atoms)

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
fresh = (letters !!) <$> next
    where letters :: [String]
          letters = [1..] >>= flip replicateM ['a'..'z']

          genName :: Int -> String
          genName i = if i < 0 then '-' : letters !! (-i) else letters !! i

next :: State Int Int
next = do
    i <- get
    put (i+1)
    return i
