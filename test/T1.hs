{-# language DeriveTraversable #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module T1 (main) where

import Test.Tasty.HUnit
import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving

import Data.Equality.Graph
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler

data TreeF a = VarF Int
             | ConstF Double
             | AddF a a
             | SubF a a
             | MulF a a
             | DivF a a
             | LogF a
               deriving (Functor, Foldable, Traversable)

deriveEq1 ''TreeF
deriveOrd1 ''TreeF
deriveShow1 ''TreeF

instance Num (Fix TreeF) where
  l + r = Fix $ AddF l r
  l - r = Fix $ SubF l r
  l * r = Fix $ MulF l r
  abs   = undefined

  negate t    = fromInteger (-1) * t
  signum t    = undefined
  fromInteger = Fix . ConstF . fromInteger

instance Fractional (Fix TreeF) where
    (/) a b = Fix (DivF a b)
    fromRational = Fix . ConstF . fromRational

instance Floating (Fix TreeF) where
  pi      = undefined
  exp     = undefined
  log     = Fix . LogF
  sqrt    = undefined
  sin     = undefined
  cos     = undefined
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

  l ** r      = undefined
  logBase l r = undefined

instance Num (Pattern TreeF) where
  l + r = NonVariablePattern $ AddF l r
  l - r = NonVariablePattern $ SubF l r
  l * r = NonVariablePattern $ MulF l r
  abs   = undefined

  negate t    = fromInteger (-1) * t
  signum t    = undefined
  fromInteger = NonVariablePattern . ConstF . fromInteger

instance Fractional (Pattern TreeF) where
    (/) a b = NonVariablePattern (DivF a b)
    fromRational = NonVariablePattern . ConstF . fromRational

instance Floating (Pattern TreeF) where
  pi      = undefined
  exp     = undefined
  log     = NonVariablePattern . LogF
  sqrt    = undefined
  sin     = undefined
  cos     = undefined
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

  l ** r      = undefined
  logBase l r = undefined

cost :: CostFunction TreeF Int
cost = \case
  ConstF _ -> 5
  VarF _ -> 1
  AddF c1 c2 -> c1 + c2 + 2
  SubF c1 c2 -> c1 + c2 + 2
  MulF c1 c2 -> c1 + c2 + 4
  DivF c1 c2 -> c1 + c2 + 5
  LogF c -> c + 2

tmpRewrites :: [Rewrite () TreeF]
tmpRewrites = [
        "x" + "y" := "y" + "x"
      , "x" * "y" := "y" * "x"
      , "x" + ("y" + "z") := ("x" + "y") + "z"
      , "x" * ("y" * "z") := ("x" * "y") * "z"
      , "x" * ("y" / "z") := ("x" * "y") / "z"
      , "x" + 0 := "x"
      , "x" * 1 := "x"
      , "x" * 0 := 0
      , "x" / "x" := 1
      , ("x" * "y") + ("x" * "z") := "x" * ("y" + "z") 
      , negate ("x" + "y") := negate "x" - "y"
      , 0 - "x" := negate "x"
      , log ("x" * "y") := log "x" + log "y"
      , log ("x" / "y") := log "x" - log "y"
      , log 1 := 0
    ]

rewriteTree :: Fix TreeF -> (Fix TreeF, EGraph () TreeF)
rewriteTree t = equalitySaturation' (BackoffScheduler 1000 15) t tmpRewrites cost

x, y :: Fix TreeF
x = Fix (VarF 0)
y = Fix (VarF 1)

main :: IO ()
main = do
  fst (rewriteTree ((log x) / (x * ((y / y) / y)))) @?= (Fix $ DivF (Fix $ LogF (Fix $ VarF 0)) (Fix $ DivF (Fix $ VarF 0) (Fix $ VarF 1)))
  pure ()

