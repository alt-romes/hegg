{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
module T45 where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Equality.Utils
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler

data SymExpr a = Const Double
               | Symbol String
               | a :+: a
               deriving (Functor, Foldable, Traversable, Eq, Ord, Show)
infix 6 :+:

-- | Test that banned rules get retried when no other rules can match (#45).
--
-- This test uses a single self-feeding rule (right-associativity) with a very
-- aggressive scheduler. The rule creates new patterns for itself:
-- @
-- (a+b)+(c+d) -> a+(b+(c+d))
-- @
-- but gets banned after iteration 0 before it can process the new patterns.
--
-- Scenario:
-- - Expression: ((a + b) + c) + d
-- - Rule: (x + y) + z := x + (y + z)  (right-associativity)
-- - Scheduler: matchLimit=1, banLength=30
-- - Cost function: prefers right-association (left subtree costs 2x)
--
-- Iteration 0:
--   - Rule matches ((a+b)+c)+d and (a+b)+c
--   - Creates: (a+b)+(c+d) and a+(b+c)
--   - Rule gets banned (>1 match, threshold is 1)
--
-- Iteration 1:
--   - Rule is banned, returns no matches
--   - noRulesMatched = true, haveBannedRules = true
--   - (a+b)+(c+d) could match but rule is banned!
--
-- Prior to fixing #45 we would have saturated at this point,
-- extracting (a+b)+(c+d) with cost 13. However with the retry logic
-- we will see that there is a banned rule and try another iteration:
--
-- Iteration 2:
--   - Rule matches (a+b)+(c+d), creates a+(b+(c+d))
--   - Extraction picks a+(b+(c+d)) with cost 10
testT45 :: TestTree
testT45 = testCase "T45 (banned rule retry)" $
    result @?= Fix (a :+: Fix (b :+: Fix (c :+: d)))
  where
    a = Fix (Symbol "a")
    b = Fix (Symbol "b")
    c = Fix (Symbol "c")
    d = Fix (Symbol "d")
    expr = Fix (Fix (Fix (a :+: b) :+: c) :+: d)

    rules :: [Rewrite () SymExpr]
    rules = [ pat (pat ("x" :+: "y") :+: "z") := pat ("x" :+: pat ("y" :+: "z"))
            ]

    -- Cost function that prefers right-association by penalizing left subtrees
    cost' :: CostFunction () SymExpr Integer
    cost' = costOnly $ \case
      Const _ -> 1
      Symbol _ -> 1
      c1 :+: c2 -> c1 * 2 + c2 + 1

    scheduler = BackoffScheduler 1 30
    (result, _) = equalitySaturation' scheduler expr rules cost'
