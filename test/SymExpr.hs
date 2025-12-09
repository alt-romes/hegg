{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Test module for type-indexed symbolic expressions.

This module preserves the original SymExpr GADT from the reference example
and demonstrates how to use SymExprF (the base functor) with hegg's
equality saturation infrastructure.

The key types from the reference:
- SymExpr: The original recursive GADT (not used directly by hegg)
- SymExprF: The base functor suitable for equality saturation
-}
module SymExpr where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Kind (Type)

import Data.Equality.Utils.Singleton
import Data.Equality.Utils.Untyped (ErasedLang(..), Untyped(..))
import Data.Equality.Utils (Fix(..))
import Data.Equality.Graph (ClassId)
import Data.Equality.Graph.Indexed
import Data.Equality.Graph.Monad.Indexed
import Data.Equality.Matching.Indexed
import Data.Equality.Matching.Pattern.Indexed
import Data.Equality.Saturation.Rewrites.Indexed
import Data.Equality.Saturation.Indexed
import Data.Equality.Extraction.Indexed
import Data.Equality.Language.Indexed ()
import Data.Equality.Analysis.Indexed ()
import Data.Equality.Saturation.Scheduler (defaultBackoffScheduler)

--------------------------------------------------------------------------------
-- Original SymExpr Types (preserved exactly from reference)
--------------------------------------------------------------------------------

-- | Phantom types representing the type-level tags
data SymType = TyDouble | TyString
  deriving (Show, Eq, Ord)

-- | GADT for symbolic expressions with type-level tracking
data SymExpr (t :: SymType) where
  Constant :: Double -> SymExpr TyDouble
  Symbol :: forall t. String -> SymExpr t
  (:+:) :: forall t. SymExpr t -> SymExpr t -> SymExpr t
  (:*:) :: SymExpr TyDouble -> SymExpr TyDouble -> SymExpr TyDouble
  (:/:) :: SymExpr TyDouble -> SymExpr TyDouble -> SymExpr TyDouble

deriving instance Show (SymExpr t)

-- | Base functor for SymExpr, suitable for use with Fix and recursion schemes
data SymExprF (t :: SymType) r where
  ConstantF :: Double -> SymExprF TyDouble r
  SymbolF :: forall t r. String -> SymExprF t r
  (:++:) :: forall t r. r -> r -> SymExprF t r
  (:**:) :: r -> r -> SymExprF TyDouble r
  (://:) :: r -> r -> SymExprF TyDouble r

deriving instance Show r => Show (SymExprF t r)
deriving instance Functor (SymExprF t)
deriving instance Foldable (SymExprF t)
deriving instance Traversable (SymExprF t)

-- | Fixed point of SymExprF (from reference)
type Expr t = Fix (SymExprF t)

-- | Example expressions (from reference)
e1 :: Expr TyDouble
e1 = Fix (Fix (ConstantF 10.0) :**: Fix (ConstantF 2.0)) -- 10 * 2

e2 :: Expr TyDouble
e2 = Fix (Fix (ConstantF 20.0) ://: Fix (ConstantF 2.0)) -- 20 / 2

e3 :: Expr TyDouble
e3 = Fix (Fix (ConstantF 5.0) :++: Fix (ConstantF 3.0)) -- 5 + 3

e4 :: Expr TyDouble
e4 = Fix (Fix (Fix (SymbolF "x") :**: Fix (ConstantF 2.0)) ://: Fix (ConstantF 2.0)) -- (x*2)/2

--------------------------------------------------------------------------------
-- Singleton Infrastructure for SymType (required by hegg)
--------------------------------------------------------------------------------

-- | Singleton types for SymType
data instance Sing (t :: SymType) where
    STyDouble :: Sing 'TyDouble
    STyString :: Sing 'TyString

instance Show (Sing (t :: SymType)) where
    show STyDouble = "STyDouble"
    show STyString = "STyString"

instance SingI 'TyDouble where
    sing = STyDouble

instance SingI 'TyString where
    sing = STyString

instance SDecide SymType where
    decEq STyDouble STyDouble = Just Refl
    decEq STyString STyString = Just Refl
    decEq _ _ = Nothing

instance SOrd SymType where
    sCompare STyDouble STyDouble = SEQ
    sCompare STyDouble STyString = SLT
    sCompare STyString STyDouble = SGT
    sCompare STyString STyString = SEQ

--------------------------------------------------------------------------------
-- Additional Instances for SymExprF (required by hegg's Language constraint)
--------------------------------------------------------------------------------

-- | Eq instance for SymExprF
instance Eq r => Eq (SymExprF t r) where
    ConstantF d1 == ConstantF d2 = d1 == d2
    SymbolF s1 == SymbolF s2 = s1 == s2
    (a1 :++: b1) == (a2 :++: b2) = a1 == a2 && b1 == b2
    (a1 :**: b1) == (a2 :**: b2) = a1 == a2 && b1 == b2
    (a1 ://: b1) == (a2 ://: b2) = a1 == a2 && b1 == b2
    _ == _ = False

-- | Ord instance for SymExprF (required for Language constraint)
instance Ord r => Ord (SymExprF t r) where
    compare (ConstantF d1) (ConstantF d2) = compare d1 d2
    compare (SymbolF s1) (SymbolF s2) = compare s1 s2
    compare (a1 :++: b1) (a2 :++: b2) = compare (a1, b1) (a2, b2)
    compare (a1 :**: b1) (a2 :**: b2) = compare (a1, b1) (a2, b2)
    compare (a1 ://: b1) (a2 ://: b2) = compare (a1, b1) (a2, b2)
    compare x y = compare (tag x) (tag y)
      where
        tag :: SymExprF d s -> Int
        tag ConstantF{} = 0
        tag SymbolF{} = 1
        tag (:++:){} = 2
        tag (:**:){} = 3
        tag (://:){} = 4

--------------------------------------------------------------------------------
-- Cost Function for Extraction
--------------------------------------------------------------------------------

-- | Cost function: prefer simpler expressions
costI :: CostFunction (ErasedLang SymExprF) Int
costI (ErasedLang (Untyped node)) = case node of
    ConstantF _ -> 1
    SymbolF _ -> 1
    _ :++: c2 -> c2 + 2
    c1 :**: c2 -> c1 + c2 + 3
    c1 ://: c2 -> c1 + c2 + 4

--------------------------------------------------------------------------------
-- Rewrite Rules
--------------------------------------------------------------------------------

-- | Algebraic rewrite rules for symbolic simplification
rewritesI :: [SomeRewrite () SymExprF]
rewritesI =
    [ -- Commutativity of multiplication
      ruleI (patI (("x" :: PatternI SymExprF 'TyDouble) :**: "y"))
            (patI (("y" :: PatternI SymExprF 'TyDouble) :**: "x"))

    , -- Associativity of multiplication: (x * y) * z = x * (y * z)
      ruleI (patI (patI (("x" :: PatternI SymExprF 'TyDouble) :**: "y") :**: "z"))
            (patI ("x" :**: patI (("y" :: PatternI SymExprF 'TyDouble) :**: "z")))

    , -- Identity: x * 1 = x
      ruleI (patI (("x" :: PatternI SymExprF 'TyDouble) :**: patI (ConstantF 1))) "x"

    , -- Identity: 1 * x = x
      ruleI (patI (patI (ConstantF 1) :**: ("x" :: PatternI SymExprF 'TyDouble))) "x"

    , -- Zero: x * 0 = 0
      ruleI (patI (("x" :: PatternI SymExprF 'TyDouble) :**: patI (ConstantF 0))) (patI (ConstantF 0))

    , -- Zero: 0 * x = 0
      ruleI (patI (patI (ConstantF 0) :**: ("x" :: PatternI SymExprF 'TyDouble))) (patI (ConstantF 0))

    , -- Self-division: x / x = 1
      ruleI (patI (("x" :: PatternI SymExprF 'TyDouble) ://: "x")) (patI (ConstantF 1))

    , -- Division identity: x / 1 = x
      ruleI (patI (("x" :: PatternI SymExprF 'TyDouble) ://: patI (ConstantF 1))) "x"

    , -- Division associativity: (a * b) / c = a * (b / c)
      ruleI (patI (patI (("a" :: PatternI SymExprF 'TyDouble) :**: "b") ://: "c"))
            (patI ("a" :**: patI (("b" :: PatternI SymExprF 'TyDouble) ://: "c")))

    , -- Division by multiplication: (a / b) / c = a / (b * c)
      ruleI (patI (patI (("a" :: PatternI SymExprF 'TyDouble) ://: "b") ://: "c"))
            (patI ("a" ://: patI (("b" :: PatternI SymExprF 'TyDouble) :**: "c")))
    ]

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

type SymExprGraph = EGraphI () SymExprF

runSymExpr :: EGraphIM () SymExprF a -> (a, SymExprGraph)
runSymExpr = egraphI

--------------------------------------------------------------------------------
-- Test Suite
--------------------------------------------------------------------------------

symExprTests :: TestTree
symExprTests = testGroup "Type-Indexed Symbolic Expressions"
    [ testGroup "Singleton Infrastructure"
        [ testCase "sing @TyDouble returns STyDouble" $
            show (sing :: Sing 'TyDouble) @?= "STyDouble"

        , testCase "sing @TyString returns STyString" $
            show (sing :: Sing 'TyString) @?= "STyString"

        , testCase "decEq STyDouble STyDouble = Just Refl" $
            case decEq STyDouble STyDouble of
                Just Refl -> return ()
                Nothing -> assertFailure "Expected Just Refl"

        , testCase "decEq STyDouble STyString = Nothing" $
            decEq STyDouble STyString @?= Nothing

        , testCase "SOrd: TyDouble < TyString" $
            case sCompare STyDouble STyString of
                SLT -> return ()
                _ -> assertFailure "Expected SLT"
        ]

    , testGroup "Original SymExpr GADT"
        [ testCase "Constant creates TyDouble expression" $ do
            let expr :: SymExpr TyDouble
                expr = Constant 42.0
            case expr of
                Constant d -> d @?= 42.0

        , testCase "Symbol is polymorphic" $ do
            let exprD :: SymExpr TyDouble
                exprD = Symbol "x"
                exprS :: SymExpr TyString
                exprS = Symbol "y"
            case (exprD, exprS) of
                (Symbol sx, Symbol sy) -> do
                    sx @?= "x"
                    sy @?= "y"

        , testCase "(:+:) is polymorphic" $ do
            let expr :: SymExpr TyDouble
                expr = Symbol "x" :+: Constant 1.0
            case expr of
                _ :+: _ -> return ()

        , testCase "(:*:) restricted to TyDouble" $ do
            let expr :: SymExpr TyDouble
                expr = Constant 2.0 :*: Constant 3.0
            case expr of
                _ :*: _ -> return ()
        ]

    , testGroup "SymExprF Base Functor"
        [ testCase "ConstantF creates TyDouble functor" $ do
            let node :: SymExprF 'TyDouble Int
                node = ConstantF 42.0
            case node of
                ConstantF d -> d @?= 42.0

        , testCase "SymbolF is polymorphic" $ do
            let nodeD :: SymExprF 'TyDouble Int
                nodeD = SymbolF "x"
                nodeS :: SymExprF 'TyString Int
                nodeS = SymbolF "y"
            case (nodeD, nodeS) of
                (SymbolF sx, SymbolF sy) -> do
                    sx @?= "x"
                    sy @?= "y"

        , testCase "Functor instance works" $ do
            let node :: SymExprF 'TyDouble Int
                node = 1 :**: 2
                mapped = fmap (+10) node
            case mapped of
                11 :**: 12 -> return ()
                _ -> assertFailure "Expected mapped values"
        ]

    , testGroup "EGraphI Operations"
        [ testCase "addI adds constant" $ do
            let (cid, _) = runSymExpr $ addIM (ConstantF 42.0)
            cid @?= 1

        , testCase "addI adds symbol" $ do
            let (cid, _) = runSymExpr $ addIM (SymbolF @'TyDouble "x")
            cid @?= 1

        , testCase "addI builds compound expression" $ do
            let (cid, _) = runSymExpr $ do
                    c1 <- addIM (SymbolF @'TyDouble "x")
                    c2 <- addIM (ConstantF 2.0)
                    addIM (c1 :**: c2)
            cid @?= 3

        , testCase "identical expressions share e-class" $ do
            let (result, _) = runSymExpr $ do
                    c1 <- addIM (ConstantF 42.0)
                    c2 <- addIM (ConstantF 42.0)
                    return (c1, c2)
            fst result @?= snd result
        ]

    , testGroup "Pattern Matching"
        [ testCase "patI creates non-variable patterns" $ do
            let p :: PatternI SymExprF 'TyDouble
                p = patI (ConstantF 42.0)
            case p of
                NonVariablePatternI _ -> return ()
                _ -> assertFailure "Expected NonVariablePatternI"

        , testCase "string literal creates variable patterns" $ do
            let p :: PatternI SymExprF 'TyDouble
                p = "x"
            case p of
                VariablePatternI "x" -> return ()
                _ -> assertFailure "Expected VariablePatternI"

        , testCase "nested patterns work" $ do
            let p :: PatternI SymExprF 'TyDouble
                p = patI ("x" :**: patI (ConstantF 2.0))
            case p of
                NonVariablePatternI _ -> return ()
                _ -> assertFailure "Expected NonVariablePatternI"
        ]

    , testGroup "Algebraic Rewrites"
        [ testCase "commutativity: multiplication x * y = y * x" $ do
            let (matches, _) = runSymExpr $ do
                    cx <- addIM (SymbolF @'TyDouble "x")
                    c2 <- addIM (ConstantF 2.0)
                    cxy <- addIM (cx :**: c2)
                    rebuildIM
                    eg <- getEGraphIM
                    let eg' = runEqualitySaturationI defaultBackoffScheduler rewritesI eg
                    put eg'
                    cyx <- addIM (c2 :**: cx)
                    rebuildIM
                    eg'' <- getEGraphIM
                    let r1 = findI eg'' cxy
                        r2 = findI eg'' cyx
                    return (r1, r2)
            fst matches @?= snd matches

        , testCase "identity: x * 1 = x" $ do
            let (result, _) = runSymExpr $ do
                    cx <- addIM (SymbolF @'TyDouble "x")
                    c1 <- addIM (ConstantF 1.0)
                    cExpr <- addIM (cx :**: c1)
                    rebuildIM
                    eg <- getEGraphIM
                    let eg' = runEqualitySaturationI defaultBackoffScheduler rewritesI eg
                    put eg'
                    let r1 = findI eg' cx
                        r2 = findI eg' cExpr
                    return (r1, r2)
            fst result @?= snd result

        , testCase "self-division: x / x = 1" $ do
            let (result, _) = runSymExpr $ do
                    cx <- addIM (SymbolF @'TyDouble "x")
                    cExpr <- addIM (cx ://: cx)
                    c1 <- addIM (ConstantF 1.0)
                    rebuildIM
                    eg <- getEGraphIM
                    let eg' = runEqualitySaturationI defaultBackoffScheduler rewritesI eg
                    put eg'
                    let r1 = findI eg' cExpr
                        r2 = findI eg' c1
                    return (r1, r2)
            fst result @?= snd result

        , testCase "zero: x * 0 = 0" $ do
            let (result, _) = runSymExpr $ do
                    cx <- addIM (SymbolF @'TyDouble "x")
                    c0 <- addIM (ConstantF 0.0)
                    cExpr <- addIM (cx :**: c0)
                    rebuildIM
                    eg <- getEGraphIM
                    let eg' = runEqualitySaturationI defaultBackoffScheduler rewritesI eg
                    put eg'
                    let r1 = findI eg' cExpr
                        r2 = findI eg' c0
                    return (r1, r2)
            fst result @?= snd result
        ]

    , testGroup "Complex Simplification"
        [ testCase "(x * 2) / 2 simplifies to x (using e4 pattern)" $ do
            -- This mirrors e4 from the reference: (x*2)/2
            let (rootId, eg) = runSymExpr $ do
                    cx <- addIM (SymbolF @'TyDouble "x")
                    c2 <- addIM (ConstantF 2.0)
                    cMul <- addIM (cx :**: c2)
                    c2' <- addIM (ConstantF 2.0)
                    addIM (cMul ://: c2')

                eg' = runEqualitySaturationI defaultBackoffScheduler rewritesI eg
                result = extractBestI eg' costI rootId

            case result of
                Fix (ErasedLang (Untyped (SymbolF "x"))) -> return ()
                _ -> assertFailure $ "Expected SymbolF x, got: " ++ show result
        ]

    , testGroup "Type Safety"
        [ testCase "type indices prevent ill-typed rewrites" $ do
            -- The following would NOT compile:
            -- badRule :: RewriteI () SymExprF 'TyDouble
            -- badRule = patI (ConstantF 1.0) :=: patI (SymbolF @'TyString "x")
            return ()

        , testCase "polymorphic operations work across types" $ do
            let nodeD :: SymExprF 'TyDouble ClassId
                nodeD = SymbolF "x"
                nodeS :: SymExprF 'TyString ClassId
                nodeS = SymbolF "y"
                addD :: SymExprF 'TyDouble ClassId
                addD = 1 :++: 2
            case (nodeD, nodeS, addD) of
                (SymbolF _, SymbolF _, _ :++: _) -> return ()

        , testCase "restricted operations maintain type safety" $ do
            let node :: SymExprF 'TyDouble ClassId
                node = 1 :**: 2
            -- The following would NOT compile:
            -- let invalid :: SymExprF 'TyString ClassId
            --     invalid = 1 :**: 2
            case node of
                _ :**: _ -> return ()
        ]
    ]
