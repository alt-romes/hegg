{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module T2 where

-- Tests whether this saturates just like mwillsey claims that it does in egg!

import Prelude hiding (not)

import Test.Tasty.HUnit
import Data.Deriving
import Data.Equality.Matching
import Data.Equality.Extraction
import Data.Equality.Saturation

data Lang a = And a a
            | Or a a
            | Not a
            | ToElim a
            | Sym Int
            deriving (Functor, Foldable, Traversable)

deriveEq1 ''Lang
deriveOrd1 ''Lang
deriveShow1 ''Lang

x, y :: Pattern Lang
x = "x"
y = "y"
not :: Pattern Lang -> Pattern Lang
not = pat . Not

rules :: [Rewrite () Lang]
rules =
  [ pat (x `And` y) := not (pat (not x `Or` not y))
  , pat (x `Or` y) := not (pat (not x `And` not y))
  , not (not x) := pat (ToElim x)
  , pat (ToElim x) := x
  ]

main :: IO ()
main = do
  fst (equalitySaturation (Fix $ (Fix $ Not $ Fix $ Sym 0) `And` (Fix $ Not $ Fix $ Sym 1)) rules depthCost) @?= Fix (Not $ Fix $ (Fix $ Sym 0) `Or` (Fix $ Sym 1))
  pure ()

