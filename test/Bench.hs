{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty.Bench

import Data.Equality.Utils
import Invariants
import Sym
import Lambda
import SimpleSym

tests :: [Benchmark]
tests = [ bgroup "Tests"
  [ symTests
  , lambdaTests
  , simpleSymTests
  , invariants
  ] ]

main :: IO ()
main = defaultMain tests
