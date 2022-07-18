{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Equality.Matching
    ( module Data.Equality.Matching
    , Subst
    )
    where

import Data.String
import Data.Maybe
import Data.Functor.Classes

import Data.List (nub)
import Data.Foldable (toList)

import Data.Fix

import Control.Monad
import Control.Monad.State

import qualified Data.Map    as M
import qualified Data.IntMap as IM

import Data.Equality.Graph

import Database

-- |  Matching a pattern on an e-graph returns substitutions for every variable
-- in the pattern and the e-class that matched the pattern
data Match = Match
    { matchSubst :: Subst
    , matchClassId :: {-# UNPACK #-} !ClassId
    }

-- | EGS version of 'ematch'
ematchM :: Language l
       => Pattern l
       -> EGS l [Match]
ematchM pat = gets (ematch pat)

-- | Match a pattern against an AST, returnin a list of equivalence classes
-- that match the pattern and the corresponding substitution
ematch :: Language l
       => Pattern l
       -> EGraph l
       -> [Match]
ematch pat eg =
    let db = eGraphToDatabase eg
        q = compileToQuery pat
     in mapMaybe copyOutEClass (genericJoin db q)
    where
        -- Given a substitution in which the first element is the pair
        -- (root_var,root_class), copy the root_class out and return it with
        -- the substitution.
        -- 
        -- That is, we return the e-class where the pattern was matched, and
        -- the list of substitutions for all variables including the root. The
        -- root variable is needed in the substitution for single variable
        -- queries to find the subst
        copyOutEClass [] = Nothing
        copyOutEClass l@((_,root_class):_) = Just $ uncurry Match (l, root_class)

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



-- | @(~x + 0) --> BinOp Add (Var "~x") (ENode (Integer 0))@
-- @~x --> VariablePattern "~x"@
data Pattern lang
    = NonVariablePattern (lang (Pattern lang))
    | VariablePattern Var

instance Eq1 l => (Eq (Pattern l)) where
    (==) (NonVariablePattern a) (NonVariablePattern b) = liftEq (==) a b
    (==) (VariablePattern a) (VariablePattern b) = a == b 
    (==) _ _ = False

instance Ord1 l => (Ord (Pattern l)) where
    compare (VariablePattern _) (NonVariablePattern _) = LT
    compare (NonVariablePattern _) (VariablePattern _) = GT
    compare (VariablePattern a) (VariablePattern b) = compare a b
    compare (NonVariablePattern a) (NonVariablePattern b) = liftCompare compare a b

instance Show1 lang => Show (Pattern lang) where
    showsPrec _ (VariablePattern s) = showString s -- ROMES:TODO don't ignore prec?
    showsPrec d (NonVariablePattern x) = liftShowsPrec showsPrec showList d x

instance IsString (Pattern lang) where
    fromString = VariablePattern

data AuxResult lang = Var :~ [Atom lang]

-- Return distinct variables in a pattern
vars :: Foldable lang => Pattern lang -> [Var]
vars (VariablePattern x) = [x]
vars (NonVariablePattern p) = nub $ join $ map vars $ toList p

compileToQuery :: (Traversable lang) => Pattern lang -> Query lang
compileToQuery = flip evalState 0 . compile_to_query'
    where
        compile_to_query' :: (Traversable lang) => Pattern lang -> State Int (Query lang)
        compile_to_query' (VariablePattern x) = return (SelectAllQuery x)
        compile_to_query' p@(NonVariablePattern _) = do
            root :~ atoms <- aux p
            return (Query (nub $ root:vars p) atoms)

        aux :: (Traversable lang) => Pattern lang -> State Int (AuxResult lang)
        aux (VariablePattern x) = return $ x :~ [] -- from definition in relational e-matching paper (needed for as base case for recursion)
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
                    subPatsToVars :: Traversable lang => lang (Pattern lang) -> [Var] -> State Int (lang Var)
                    subPatsToVars p' boundVars = traverse (const $ (boundVars !!) <$> next) p'

fresh :: State Int String
fresh = ('$':) . ('~':) . (letters !!) <$> next
    where letters :: [String]
          letters = [1..] >>= flip replicateM ['a'..'z']

next :: State Int Int
next = do
    i <- get
    put (i+1)
    return i

