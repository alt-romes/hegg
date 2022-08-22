module SimpleSym where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Equality.Matching

data SymExpr = Const Double
             | Symbol String
             | SymExpr :+: SymExpr
             | SymExpr :*: SymExpr
             | SymExpr :/: SymExpr
infix 6 :+:
infix 7 :*:, :/:

e1 :: SymExpr
e1 = ((Symbol "x") :*: Const 2) :/: (Const 2)

a, b, c, d :: Pattern SymExpr
(a, b, c, d) = ("a", "b", "c", "d")

simpleSymTests :: TestTree
simpleSymTests = testGroup "Simple Sym"
    [ testCase "(a*2)/2 = a" $ rewrite ((a*2)/2) @?= a
    ]
