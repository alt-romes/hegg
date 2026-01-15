{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Regression test for conditional rewrite banning fix (commit 2e8ba69c).
--
-- This tests that conditional rewrites are not banned prematurely when
-- they have many structural matches but the condition only passes for a few.
--
-- Before the fix, structural matches where conditions failed were counted
-- towards the backoff scheduler's ban threshold. This caused conditional
-- rewrites to be banned before they could fire for cases where conditions
-- actually pass.
module T51 where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Equality.Utils
import Data.Equality.Matching
import Data.Equality.Matching.Database (Subst, findSubst)
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler
import Data.Equality.Graph
import Data.Equality.Graph.Lens

-- | Simple expression language
data Expr a = Const Int
            | Symbol String
            | Marker          -- A marker that will be merged with Special
            | Special         -- The node that the condition checks for
            | a :+: a
            deriving (Functor, Foldable, Traversable, Eq, Ord, Show)
infix 6 :+:

cost :: CostFunction anl Expr Int
cost = costOnly $ \case
  Const _   -> 1
  Symbol _  -> 1
  Marker    -> 1
  Special   -> 1
  c1 :+: c2 -> c1 + c2 + 2

-- | Helper to get the class ID for a pattern variable
unsafeGetSubst :: Pattern Expr -> VarsState -> Subst -> ClassId
unsafeGetSubst (NonVariablePattern _) _ _ = error "unsafeGetSubst: expected VariablePattern"
unsafeGetSubst (VariablePattern v) vss subst = findSubst (findVarName vss v) subst

-- | Condition: check if the matched class contains the Special node
hasSpecial :: Pattern Expr -> RewriteCondition () Expr
hasSpecial v vss subst egr =
  any isSpecial (egr^._class (unsafeGetSubst v vss subst)._nodes)
  where
    isSpecial (Node Special) = True
    isSpecial _ = False

-- | Pattern helpers
exprPat :: Expr (Pattern Expr) -> Pattern Expr
exprPat = NonVariablePattern

-- | Build expression: Marker + (1 + (2 + (3 + ... + z)))
-- The Marker class does NOT contain Special initially.
-- After Rule 1 fires (Marker := Special), the Marker class will contain Special.
buildExpr :: Fix Expr
buildExpr = Fix (Fix Marker :+: rest)
  where
    rest = foldr (\n e -> Fix (Fix (Const n) :+: e))
                 (Fix (Symbol "z"))
                 [1..20]

-- | Rewrites:
-- Rule 1: Marker := Special (unconditional) - adds Special to Marker's class
-- Rule 2: x + y := y when hasSpecial(x) (conditional)
--
-- The key insight: rules are matched THEN applied. So on iteration 0:
--   - Matching phase: All :+: nodes match Rule 2 structurally (~21 matches)
--   - Matching phase: Marker class does NOT have Special yet
--   - Matching phase: hasSpecial(Marker) returns False for the top :+:
--   - Stats update: Without fix, 21 structural matches counted. With fix, 0.
--   - Apply phase: Rule 1 adds Special to Marker's class
--   - Apply phase: Rule 2 has no valid matches (0 passed condition)
--
-- On iteration 1:
--   - Matching phase: Marker class NOW has Special (from iteration 0)
--   - Matching phase: hasSpecial(Marker) returns True
--   - Without fix: Rule 2 is BANNED from iteration 0
--   - With fix: Rule 2 is NOT banned, can fire
rewrites :: [Rewrite () Expr]
rewrites =
  [ -- Rule 1: add Special to Marker's class (fires once, changes structure)
    exprPat Marker := exprPat Special
    -- Rule 2: remove x+ when x has Special (conditional)
  , exprPat ("x" :+: "y") := "y" :| hasSpecial "x"
  ]

-- | A scheduler with a very low match limit and long ban.
-- With matchLimit=5 and 21 matches (each with 2 vars = 42 total), the rule
-- gets banned immediately without the fix.
lowLimitScheduler :: BackoffScheduler
lowLimitScheduler = BackoffScheduler
  { matchLimit = 5
  , banLength  = 100  -- Longer than the 30-iteration limit
  }

-- | The main test.
--
-- Without fix:
--   Iteration 0: Rule 2 matches 21 times structurally, 0 pass condition.
--                Stats count 21 matches (42 total_len) > 5 → BANNED.
--                Rule 1 fires, adding Special to Marker's class.
--   Iteration 1: Rule 2 is banned. Marker+ can't be removed.
--   Result: Marker+ remains.
--
-- With fix:
--   Iteration 0: Rule 2 matches 21 times structurally, 0 pass condition.
--                Stats count 0 filtered matches (0 total_len) < 5 → not banned.
--                Rule 1 fires, adding Special to Marker's class.
--   Iteration 1: Rule 2 not banned. hasSpecial(Marker) = True. Match fires.
--   Result: Marker+ removed.
testConditionalBan :: TestTree
testConditionalBan = testGroup "Conditional Rewrite Banning"
  [ testCase "conditional rule not banned prematurely" $ do
      let expr = buildExpr
      let (result, _) = equalitySaturation' lowLimitScheduler expr rewrites cost

      -- After saturation, the Marker+ should be removed
      -- (Marker is merged with Special, then Special+ fires)
      case unFix result of
        Fix Marker :+: _ ->
          assertFailure "Expected Marker+ to be removed (rule likely banned before condition could pass)"
        Fix Special :+: _ ->
          assertFailure "Expected Special+ to be removed (rule likely banned before condition could pass)"
        _ -> return ()  -- Success

  , testCase "direct Special+a = a works" $ do
      -- Sanity check: when Special is in class from the start, rule fires
      let expr = Fix (Fix Special :+: Fix (Symbol "a"))
      let singleRewrite = [exprPat ("x" :+: "y") := "y" :| hasSpecial "x"]
      let (result, _) = equalitySaturation' lowLimitScheduler expr singleRewrite cost
      result @?= Fix (Symbol "a")
  ]
