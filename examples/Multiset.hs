{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- = Multiset A/C canonicalization with @normalizeNode@
--
-- This example shows how to use 'normalizeNode' to handle
-- associative-commutative (A/C) operators structurally, avoiding the
-- e-graph size blowup that occurs with A/C rewrite rules.
--
-- == Problem
--
-- With binary addition and A/C rewrite rules (@a+b := b+a@,
-- @a+(b+c) := (a+b)+c@), the e-graph grows exponentially:
-- a sum of /n/ terms produces O(2^n) e-classes.
--
-- == Solution
--
-- Represent addition as an n-ary operator over a /multiset/ of children.
-- Override 'normalizeNode' to sort the children after canonicalization.
-- Since the e-graph's memo map uses 'Ord' for lookups, sorted children
-- give multiset equality for free — @Add [a,b,c]@ and @Add [c,a,b]@
-- hash to the same e-node. No A/C rewrite rules needed.
--
-- == Usage
--
-- > cabal run multiset-example
--
module Main where

import Data.List (sort)

import Data.Equality.Graph hiding (add)
import Data.Equality.Graph.Lens
import Data.Equality.Extraction
import Data.Equality.Analysis
import Data.Equality.Saturation

-- | A simple numeric expression language with n-ary A/C addition and multiplication.
data Expr a
    = Var !String          -- ^ Variable
    | Num !Int             -- ^ Integer constant
    | Add ![a]             -- ^ N-ary addition (associative, commutative)
    | Mul ![a]             -- ^ N-ary multiplication (associative, commutative)
    | Neg !a               -- ^ Unary negation
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Override 'normalizeNode' to sort children of A/C operators.
-- This is all that's needed for multiset canonicalization.
instance Language Expr where
    normalizeNode (Add xs) = Add (sort xs)
    normalizeNode (Mul xs) = Mul (sort xs)
    normalizeNode other    = other

-- | Constant folding analysis: attach @Maybe Int@ to each e-class.
instance Analysis (Maybe Int) Expr where
    makeA (Num x)  = Just x
    makeA (Add ds) = foldl (\acc d -> (+) <$> acc <*> d) (Just 0) ds
    makeA (Mul ds) = foldl (\acc d -> (*) <$> acc <*> d) (Just 1) ds
    makeA (Neg d)  = negate <$> d
    makeA _        = Nothing

    joinA Nothing x = x
    joinA x Nothing = x
    joinA (Just x) (Just y)
        | x == y    = Just x
        | otherwise = Nothing

    -- When an e-class gets a constant value, add the constant e-node
    modifyA c egr = case egr ^. _class c . _data of
        Nothing -> egr
        Just i  ->
            let (c', egr') = represent (Fix (Num i)) egr
            in snd $ merge c c' egr'

-- | Cost function: prefer smaller expressions.
cost :: CostFunction (Maybe Int) Expr Int
cost = costOnly $ \case
    Num _  -> 1
    Var _  -> 1
    Neg c  -> c + 1
    Add cs -> sum cs + 1
    Mul cs -> sum cs + 1

-- | Smart constructors
var :: String -> Fix Expr
var = Fix . Var

num :: Int -> Fix Expr
num = Fix . Num

add :: [Fix Expr] -> Fix Expr
add = Fix . Add

mul :: [Fix Expr] -> Fix Expr
mul = Fix . Mul

neg :: Fix Expr -> Fix Expr
neg = Fix . Neg

main :: IO ()
main = do
    putStrLn "=== Multiset A/C Canonicalization Example ==="
    putStrLn ""

    putStrLn "1. Commutativity is automatic (no rewrite rules needed)"
    putStrLn ""

    let ab  = add [var "a", var "b"]
        ba  = add [var "b", var "a"]
        (idAB, eg1) = represent ab  (emptyEGraph :: EGraph (Maybe Int) Expr)
        (idBA, eg2) = represent ba eg1

    putStrLn $ "   a + b  class: " ++ show (find idAB eg2)
    putStrLn $ "   b + a  class: " ++ show (find idBA eg2)
    putStrLn $ "   Same class?   " ++ show (find idAB eg2 == find idBA eg2)
    putStrLn ""

    putStrLn "2. All permutations of a+b+c+d collapse to one e-class"
    putStrLn ""

    let perms = [ [var "a", var "b", var "c", var "d"]
                , [var "d", var "c", var "b", var "a"]
                , [var "b", var "d", var "a", var "c"]
                , [var "c", var "a", var "d", var "b"]
                ]
        exprs = map add perms
        (ids, eg3) = foldr
            (\expr (acc, eg) -> let (cid, eg') = represent expr eg in (cid:acc, eg'))
            ([], emptyEGraph :: EGraph (Maybe Int) Expr)
            exprs
        canonical = map (`find` eg3) ids

    putStrLn $ "   Canonical class IDs: " ++ show canonical
    putStrLn $ "   All same?           " ++ show (all (== head canonical) (tail canonical))
    putStrLn ""

    putStrLn "3. Constant folding: 2 + 3 + a = 5 + a"
    putStrLn ""

    let expr1 = add [num 2, num 3, var "a"]
        (best, _) = equalitySaturation @(Maybe Int) expr1 [] cost

    putStrLn   "   Input:  2 + 3 + a"
    putStrLn $ "   Output: " ++ showExpr best
    putStrLn ""

    putStrLn "4. Congruence: after merging a=b, (a+c) == (b+c)"
    putStrLn ""

    let ac = add [var "a", var "c"]
        bc = add [var "b", var "c"]
        (idA,  eg4)  = represent (var "a") (emptyEGraph :: EGraph (Maybe Int) Expr)
        (idB,  eg5)  = represent (var "b") eg4
        (idAC, eg6)  = represent ac eg5
        (idBC, eg7)  = represent bc eg6

    putStrLn $ "   Before merge: a+c == b+c? " ++ show (find idAC eg7 == find idBC eg7)

    let (_, eg8) = merge idA idB eg7
        eg9 = rebuild eg8

    putStrLn $ "   After merge:  a+c == b+c? " ++ show (find idAC eg9 == find idBC eg9)
    putStrLn ""

    putStrLn "5. Mixed: (a*b) + (c*d) == (d*c) + (b*a)"
    putStrLn ""

    let expr2 = add [mul [var "a", var "b"], mul [var "c", var "d"]]
        expr3 = add [mul [var "d", var "c"], mul [var "b", var "a"]]
        (id2, eg10) = represent expr2 (emptyEGraph :: EGraph (Maybe Int) Expr)
        (id3, eg11) = represent expr3 eg10

    putStrLn $ "   Same class? " ++ show (find id2 eg11 == find id3 eg11)

-- | Simple expression printer
showExpr :: Fix Expr -> String
showExpr (Fix (Var s))  = s
showExpr (Fix (Num n))  = show n
showExpr (Fix (Neg e))  = "-(" ++ showExpr e ++ ")"
showExpr (Fix (Add es)) = "(" ++ concatWith " + " (map showExpr es) ++ ")"
showExpr (Fix (Mul es)) = "(" ++ concatWith " * " (map showExpr es) ++ ")"

concatWith :: String -> [String] -> String
concatWith _ []     = ""
concatWith _ [x]    = x
concatWith sep (x:xs) = x ++ sep ++ concatWith sep xs
