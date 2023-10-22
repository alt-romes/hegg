{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception

-- import Data.Equality.Utils
import Invariants
import Sym
import Lambda
import SimpleSym

import qualified T1
import qualified T2
import qualified Jacobi

tests :: TestTree
tests = testGroup "Tests"
  [ symTests
  , lambdaTests
  , simpleSymTests
  , invariants
  , testCase "T1" (T1.main `catch` (\(e :: SomeException) -> assertFailure (show e)))
  , testCase "T2" (T2.main `catch` (\(e :: SomeException) -> assertFailure (show e)))
  , Jacobi.symTests
  ]

main :: IO ()
main = defaultMain tests

-- main :: IO ()
-- main = do
--     print $ Sym.rewrite (Fix $ BinOp Integral (Fix $ BinOp Pow "x" 1) "x")

-- main :: IO ()
-- main = do
--   print $ Sym.rewrite (_i (_ln "x") "x")
--   putStrLn "Expecting: x*ln(x) + (-1)"
