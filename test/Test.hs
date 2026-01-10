{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception

-- import Data.Equality.Utils
import Invariants
import Sym
import Lambda
import SimpleSym
import T32
import Singleton
import Indexed
import SymExpr

import qualified T1
import qualified T2
import qualified T3
# ifdef VIZDOT
import qualified VizDot
# endif

tests :: TestTree
tests = testGroup "Tests"
    [ symTests
    , lambdaTests
    , simpleSymTests
    , invariants
    , singletonTests
    , indexedTests
    , symExprTests
    , testCase "T1" (T1.main `catch` (\(e :: SomeException) -> assertFailure (show e)))
    , testCase "T2" (T2.main `catch` (\(e :: SomeException) -> assertFailure (show e)))
    , testCase "T3" (T3.main `catch` (\(e :: SomeException) -> assertFailure (show e)))
    , testT32
# ifdef VIZDOT
      , testCase "e-graph visualization" VizDot.visualizeSaturatedEGraph
# endif
    ]

main :: IO ()
main = do
   defaultMain tests

-- main :: IO ()
-- main = do
--     print $ Sym.rewrite (Fix $ BinOp Integral (Fix $ BinOp Pow "x" 1) "x")

-- main :: IO ()
-- main = do
--   print $ Sym.rewrite (_i (_ln "x") "x")
--   putStrLn "Expecting: x*ln(x) + (-1)"
