import Test.Tasty

import Invariants
import Sym

tests :: TestTree
tests = testGroup "Tests"
  [ symTests
  , invariants
  ]

main :: IO ()
main = defaultMain tests
