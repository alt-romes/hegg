{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances, DeriveAnyClass, RankNTypes, QuantifiedConstraints, UndecidableInstances, DeriveGeneric #-}
import Test.Tasty.Bench

import GHC.Generics
import Control.DeepSeq

import Data.Equality.Utils
import Invariants
import Sym

-- Instances for benchmarking. It's amazing this works!
deriving instance (forall a. Generic a => Generic (f a)) => Generic (Fix f)
deriving instance NFData UOp
deriving instance NFData BOp
deriving instance NFData a => NFData (Expr a)
deriving instance (forall a. NFData a => NFData (f a), forall a. Generic a => Generic (f a)) => NFData (Fix f)

tests :: [Benchmark]
tests = [ bgroup "Tests"
  [ bgroup "Symbolic bench"
    [ bench "i1" $
        nf rewrite (Fix $ BinOp Integral 1 "x")

    , bench "i2" $
        nf rewrite (Fix $ BinOp Integral (Fix $ UnOp Cos "x") "x")

    , bench "i3" $
        nf rewrite (Fix $ BinOp Integral (Fix $ BinOp Pow "x" 1) "x")

    , bench "i4" $
        nf rewrite (_i ((*) "x" (_cos "x")) "x")

    , bench "i5" $
        nf rewrite (_i ((*) (_cos "x") "x") "x")

    , bench "i6" $
        nf rewrite (_i (_ln "x") "x")
    ]
  -- , invariants
  ] ]

main :: IO ()
main = defaultMain tests
