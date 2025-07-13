{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
module T32 where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Equality.Utils
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Graph
import qualified Data.Equality.Graph as EG
import Data.Equality.Saturation.Scheduler (defaultBackoffScheduler)
import Data.Equality.Graph.Monad

data SymExpr a = Symbol String
               | a :+: a
               deriving (Functor, Foldable, Traversable, Eq, Ord, Show)
infix 6 :+:

-- This test tests that using "VariablePattern 1, VariablePattern 2,
-- VariablePattern 3" in a rewrite rule succeeds, as opposed to using the
-- IsString instance for Pattern.
-- (Well, it tests that we can no longer screw up by writing the numbers
-- directly as well. Now the implementation transforms the strings into Ids
-- when compiling the pattern)
rewrites :: [Rewrite () SymExpr]
rewrites =
  [ pat (VariablePattern "1" :+: pat (VariablePattern "2" :+: VariablePattern "3")) := pat (pat (VariablePattern "1" :+: VariablePattern "2") :+: VariablePattern "3")
  ]

e1, e1' :: Fix SymExpr
e1 = Fix (Fix (Fix (Symbol "a") :+: Fix (Symbol "b")) :+: Fix (Symbol "c"))
e1' = Fix (Fix (Symbol "a") :+: Fix (Fix (Symbol "b") :+: Fix (Symbol "c")))

somePattern :: Pattern []
somePattern = NonVariablePattern [VariablePattern "0",VariablePattern "1"] 

-- Test basic associativity using pattern vars
testT32 :: TestTree
testT32 = testGroup "T32"
    [ testCase "basic associativity with VarPattern 1,2,3" $
        let
          (e1_id, eg0)  = EG.represent @() e1 emptyEGraph
          (e1'_id, eg1) = EG.represent @() e1' eg0
          ((), eg2)     = runEGraphM eg1 (runEqualitySaturation defaultBackoffScheduler rewrites)
          e1_canon      = EG.find e1_id eg2
          e1'_canon     = EG.find e1'_id eg2
         in e1_canon @?= e1'_canon
    -- , testCase "compiling pattern" $
    --     compileToQuery somePattern @?= Query ...
    ]
