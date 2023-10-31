{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
module SimpleSym where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Equality.Utils
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Analysis
import Data.Equality.Graph
import Data.Equality.Graph.Lens

data SymExpr a = Const Double
               | Symbol String
               | a :+: a
               | a :*: a
               | a :/: a
               deriving (Functor, Foldable, Traversable, Eq, Ord, Show)
infix 6 :+:
infix 7 :*:, :/:

instance Analysis (Maybe Double) SymExpr where
  makeA = \case
    Const x -> Just x
    Symbol _ -> Nothing
    x :+: y -> (+) <$> x <*> y
    x :*: y -> (*) <$> x <*> y
    x :/: y -> (/) <$> x <*> y

  joinA Nothing (Just x) = Just x
  joinA (Just x) Nothing = Just x
  joinA Nothing Nothing  = Nothing
  joinA (Just x) (Just y) = if x == y then Just x else error "ouch, that shouldn't have happened"

  modifyA c eg
    = case eg^._class c._data of
        Nothing -> eg
        Just i  ->
          let (c', eg') = represent (Fix (Const i)) eg
           in snd $ merge c c' eg'

cost :: CostFunction SymExpr Int
cost = \case
  Const  _ -> 1
  Symbol _ -> 1
  c1 :+: c2 -> c1 + c2 + 2
  c1 :*: c2 -> c1 + c2 + 3
  c1 :/: c2 -> c1 + c2 + 4

rewrites :: [Rewrite (Maybe Double) SymExpr]
rewrites =
  [ pat (pat ("a" :*: "b") :/: "c") := pat ("a" :*: pat ("b" :/: "c"))
  , pat ("x" :/: "x")               := pat (Const 1)
  , pat ("x" :*: pat (Const 1))     := "x"
  ]

rewrite :: Fix SymExpr -> Fix SymExpr
rewrite e = fst (equalitySaturation e rewrites cost)

e1 :: Fix SymExpr
e1 = Fix (Fix (Fix (Symbol "x") :*: Fix (Const 2)) :/: Fix (Const 2)) -- (x*2)/2

simpleSymTests :: TestTree
simpleSymTests = testGroup "Simple Sym"
    [ testCase "(a*2)/2 = a"  $ rewrite e1 @?= Fix (Symbol "x")
    , testCase "(x/x)+1) = 4" $ rewrite (Fix $ Fix (Const 3) :+: Fix (Fix (Symbol "x") :/: Fix (Symbol "x"))) @?= Fix (Const 4)
    ]
