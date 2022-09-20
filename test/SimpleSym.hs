{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module SimpleSym where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving

import Data.Equality.Utils
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Language
import Data.Equality.Analysis

data SymExpr a = Const Double
               | Symbol String
               | a :+: a
               | a :*: a
               | a :/: a
               deriving (Functor, Foldable, Traversable)
infix 6 :+:
infix 7 :*:, :/:

deriveEq1   ''SymExpr
deriveOrd1  ''SymExpr
deriveShow1 ''SymExpr

data SE
instance Analysis SE SymExpr where
  type Domain SE SymExpr = ()
  makeA _ = ()
  joinA _ _ = ()

instance Language SymExpr

cost :: CostFunction SymExpr Int
cost = \case
  Const  _ -> 1
  Symbol _ -> 1
  c1 :+: c2 -> c1 + c2 + 2
  c1 :*: c2 -> c1 + c2 + 3
  c1 :/: c2 -> c1 + c2 + 4

rewrites :: [Rewrite SE SymExpr]
rewrites =
  [ pat (pat ("a" :*: "b") :/: "c") := pat ("a" :*: pat ("b" :/: "c"))
  , pat ("x" :/: "x")               := pat (Const 1)
  , pat ("x" :*: pat (Const 1))   := "x"
  ]

rewrite :: Fix SymExpr -> Fix SymExpr
rewrite e = fst (equalitySaturation e rewrites cost)

e1 :: Fix SymExpr
e1 = Fix (Fix (Fix (Symbol "x") :*: Fix (Const 2)) :/: Fix (Const 2)) -- (x*2)/2

simpleSymTests :: TestTree
simpleSymTests = testGroup "Simple Sym"
    [ testCase "(a*2)/2 = a" $ rewrite e1 @?= Fix (Symbol "x")
    ]
