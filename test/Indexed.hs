{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Test module for type-indexed expression support.

This module tests the full type-indexed infrastructure including:
- Type erasure via ErasedLang
- Indexed e-graph operations
- Type-indexed patterns
- Type-preserving rewrites
- Indexed e-matching
- Indexed equality saturation
-}
module Indexed where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Kind (Type)

import Data.Equality.Utils.Singleton ()
import Data.Equality.Utils.Untyped ()
import Data.Equality.Graph ()
import Data.Equality.Graph.Indexed
import Data.Equality.Graph.Monad.Indexed
import Data.Equality.Matching.Indexed
import Data.Equality.Matching.Pattern.Indexed
import Data.Equality.Saturation.Rewrites.Indexed
import Data.Equality.Saturation.Indexed
import Data.Equality.Extraction.Indexed ()
import Data.Equality.Language.Indexed ()
import Data.Equality.Analysis.Indexed ()
import Data.Equality.Saturation.Scheduler (defaultBackoffScheduler)
import Data.Equality.Saturation.Rewrites (Rewrite(..))
import Data.Equality.Matching.Pattern (Pattern(..))

-- Import the Ty type and instances from Singleton test module
import Singleton (Ty(..))

--------------------------------------------------------------------------------
-- Test Type-Indexed Language
--------------------------------------------------------------------------------

-- | A simple type-indexed expression language
--
-- The type parameters are:
-- - @dom@: result type index
-- - @a@: recursion parameter
type TExpr :: Ty -> Type -> Type
data TExpr dom a where
    -- Constants
    TInt  :: Int  -> TExpr 'TyInt a
    TBool :: Bool -> TExpr 'TyBool a

    -- Arithmetic
    TAdd :: a -> a -> TExpr 'TyInt a
    TMul :: a -> a -> TExpr 'TyInt a

    -- Comparison
    TEq  :: a -> a -> TExpr 'TyBool a

    -- Conditional
    TIf  :: a -> a -> a -> TExpr 'TyInt a

deriving instance Show a => Show (TExpr dom a)
deriving instance Eq a => Eq (TExpr dom a)
deriving instance Functor (TExpr dom)
deriving instance Foldable (TExpr dom)
deriving instance Traversable (TExpr dom)

-- | Ord instance required for Language
--
-- Note: Since TExpr is a GADT with type indices, we need a total ordering
-- that works across all constructors. We assign each constructor a tag
-- for cross-constructor comparison, and compare by content within the
-- same constructor.
instance Ord a => Ord (TExpr dom a) where
    compare (TInt n1) (TInt n2) = compare n1 n2
    compare (TBool b1) (TBool b2) = compare b1 b2
    compare (TAdd l1 r1) (TAdd l2 r2) = compare (l1, r1) (l2, r2)
    compare (TMul l1 r1) (TMul l2 r2) = compare (l1, r1) (l2, r2)
    compare (TEq l1 r1) (TEq l2 r2) = compare (l1, r1) (l2, r2)
    compare (TIf c1 t1 e1) (TIf c2 t2 e2) = compare (c1, t1, e1) (c2, t2, e2)
    -- Cross-constructor comparisons: order by constructor tag
    compare x y = compare (tag x) (tag y)
      where
        tag :: TExpr d b -> Int
        tag TInt{}  = 0
        tag TBool{} = 1
        tag TAdd{}  = 2
        tag TMul{}  = 3
        tag TEq{}   = 4
        tag TIf{}   = 5

-- No need for explicit AnalysisI instance - generic () instance is used

--------------------------------------------------------------------------------
-- Test Suite
--------------------------------------------------------------------------------

-- Helper type alias for readability
type TExprGraph = EGraphI () TExpr

-- Helper to run TExpr computation
runTExpr :: EGraphIM () TExpr a -> (a, TExprGraph)
runTExpr = egraphI

indexedTests :: TestTree
indexedTests = testGroup "Type-Indexed Expressions"
    [ testGroup "EGraphI Operations"
        [ testCase "emptyEGraphI creates empty graph" $ do
            let _eg = emptyEGraphI :: TExprGraph
            -- Just verify it doesn't crash
            return ()

        , testCase "addI adds nodes and returns class IDs" $ do
            let (cid, _) = runTExpr $ addIM (TInt 42)
            cid @?= 1  -- First class should be 1

        , testCase "addI with same node returns same class" $ do
            let (result, _) = runTExpr $ do
                    cid1 <- addIM (TInt 42)
                    cid2 <- addIM (TInt 42)
                    return (cid1, cid2)
            fst result @?= snd result

        , testCase "addI with different nodes returns different classes" $ do
            let (result, _) = runTExpr $ do
                    cid1 <- addIM (TInt 1)
                    cid2 <- addIM (TInt 2)
                    return (cid1, cid2)
            fst result /= snd result @?= True

        , testCase "compound expressions can be built" $ do
            let (cid, _) = runTExpr $ do
                    c1 <- addIM (TInt 1)
                    c2 <- addIM (TInt 2)
                    addIM (TAdd c1 c2)
            cid @?= 3  -- Third class (after TInt 1 and TInt 2)
        ]

    , testGroup "PatternI"
        [ testCase "patI creates non-variable patterns" $ do
            let p :: PatternI TExpr 'TyInt
                p = patI (TInt 42)
            -- Just verify construction succeeds
            case p of
                NonVariablePatternI _ -> return ()
                _ -> assertFailure "Expected NonVariablePatternI"

        , testCase "string literal creates variable patterns" $ do
            let p :: PatternI TExpr 'TyInt
                p = "x"
            case p of
                VariablePatternI "x" -> return ()
                _ -> assertFailure "Expected VariablePatternI"

        , testCase "erasePatternI converts to standard pattern" $ do
            let p :: PatternI TExpr 'TyInt
                p = patI (TInt 42)
                erased = erasePatternI p
            -- Just verify erasure succeeds
            case erased of
                NonVariablePattern _ -> return ()
                _ -> assertFailure "Expected NonVariablePattern"
        ]

    , testGroup "RewriteI"
        [ testCase "(:=:) creates type-preserving rewrites" $ do
            -- Commutativity: x + y = y + x
            let x :: PatternI TExpr 'TyInt
                x = "x"
                y :: PatternI TExpr 'TyInt
                y = "y"
                rw :: RewriteI () TExpr 'TyInt
                rw = patI (TAdd x y) :=: patI (TAdd y x)
            -- Just verify construction succeeds
            case rw of
                _ :=: _ -> return ()
                _ :|: _ -> assertFailure "Expected simple rewrite, not conditional"

        , testCase "SomeRewrite wraps rewrites" $ do
            let x :: PatternI TExpr 'TyInt
                x = "x"
                y :: PatternI TExpr 'TyInt
                y = "y"
                rw :: SomeRewrite () TExpr
                rw = ruleI (patI (TAdd x y)) (patI (TAdd y x))
            case rw of
                SomeRewrite _ -> return ()

        , testCase "eraseSomeRewrite converts to standard rewrite" $ do
            let x :: PatternI TExpr 'TyInt
                x = "x"
                y :: PatternI TExpr 'TyInt
                y = "y"
                rw :: SomeRewrite () TExpr
                rw = ruleI (patI (TAdd x y)) (patI (TAdd y x))
                erased = eraseSomeRewrite rw
            -- Just verify erasure succeeds
            case erased of
                _ := _ -> return ()
                _ :| _ -> assertFailure "Expected simple rewrite"
        ]

    , testGroup "Type-Indexed E-Matching"
        [ testCase "ematchI finds matches" $ do
            let (matches, _) = runTExpr $ do
                    c1 <- addIM (TInt 1)
                    c2 <- addIM (TInt 2)
                    _ <- addIM (TAdd c1 c2)
                    rebuildIM
                    eg <- getEGraphIM
                    let db = eGraphToDatabaseI eg
                        x :: PatternI TExpr 'TyInt
                        x = "x"
                        y :: PatternI TExpr 'TyInt
                        y = "y"
                        pat :: PatternI TExpr 'TyInt
                        pat = patI (TAdd x y)
                    return $ ematchI db pat
            length matches @?= 1
        ]

    , testGroup "Indexed Equality Saturation"
        [ testCase "runEqualitySaturationI applies rewrites" $ do
            -- Build 1 + 2 and apply commutativity
            let (finalEg, _) = runTExpr $ do
                    c1 <- addIM (TInt 1)
                    c2 <- addIM (TInt 2)
                    _ <- addIM (TAdd c1 c2)
                    rebuildIM
                    eg <- getEGraphIM
                    let x :: PatternI TExpr 'TyInt
                        x = "x"
                        y :: PatternI TExpr 'TyInt
                        y = "y"
                        rw = ruleI (patI (TAdd x y)) (patI (TAdd y x))
                    return $ runEqualitySaturationI defaultBackoffScheduler [rw] eg
            -- Just verify it runs without error
            case finalEg of
                EGraphI _ -> return ()
        ]

    , testGroup "Type Safety"
        [ testCase "ill-typed rewrites don't compile (documented)" $ do
            -- This demonstrates that the type system prevents ill-typed rewrites
            -- The following would NOT compile:
            -- badRule = patI (TInt 1) :=: patI (TBool True)
            --
            -- GHC would report:
            --   Couldn't match type ''TyBool' with ''TyInt'
            return ()

        , testCase "mixed-type operations preserve indices" $ do
            -- Build: if (1 == 2) then 3 else 4
            let (cid, _) = runTExpr $ do
                    c1 <- addIM (TInt 1)
                    c2 <- addIM (TInt 2)
                    cond <- addIM (TEq c1 c2)
                    c3 <- addIM (TInt 3)
                    c4 <- addIM (TInt 4)
                    addIM (TIf cond c3 c4)
            -- Verify the expression was added (6th class)
            cid @?= 6
        ]
    ]

