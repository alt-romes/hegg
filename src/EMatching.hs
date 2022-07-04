{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module EMatching where

import Data.String
import Data.Maybe

import Data.Foldable (toList)

import Control.Monad
import Control.Monad.State

import EGraph.ENode
import EGraph.EClass
import EGraph

type Var = String

-- | @(~x + 0) --> BinOp Add (Var "~x") (ENode (Integer 0))@
-- @~x --> VariablePattern "~x"@
data PatternAST lang = NonVariablePattern (lang (PatternAST lang)) | VariablePattern Var
deriving instance Show (lang (PatternAST lang)) => Show (PatternAST lang)

instance IsString (PatternAST lang) where
    fromString = VariablePattern

-- | An Atom ... in pattern ... is R_f(v, v1, ..., vk), so we define it as a
-- functor ast over pattern variables + the additional var for the e-class id
data Atom lang = Atom Var (lang Var)
deriving instance Show (lang Var) => Show (Atom lang)

data Query lang = Query [Var] [Atom lang]
deriving instance Show (lang Var) => Show (Query lang)

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
            return (v :~ (Atom v p':atoms))
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
