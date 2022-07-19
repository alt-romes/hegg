import Test.Tasty

import Invariants
import Sym
import Lambda

tests :: TestTree
tests = testGroup "Tests"
  [ symTests
  , lambdaTests
  , invariants
  ]

main :: IO ()
main = defaultMain tests
