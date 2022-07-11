{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Sym where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.String

import Data.Functor.Classes

import Data.Equality.Graph
import Data.Equality.Matching
import Data.Equality.Saturation

data Expr a = Sym String
            | Const Double

            | BinOp Op a a

            | Sin a
            | Cos a
            | Sqrt a
            | Ln a

            | Diff a a
            | Integral a a
            deriving ( Eq,  Ord, Functor
                     , Foldable, Traversable
                     )

instance Eq1 Expr where
    liftEq eq a b = case (a, b) of
        (Sym x, Sym y) -> x == y
        (Const x, Const y) -> x == y
        (BinOp op x y, BinOp op' x' y') -> op == op' && x `eq` x' && y `eq` y'
        (Sin a, Sin b) -> a `eq` b
        (Cos a, Cos b) -> a `eq` b
        (Sqrt a, Sqrt b) -> a `eq` b
        (Ln a, Ln b) -> a `eq` b
        (Diff a b, Diff a' b') -> a `eq` a' && b `eq` b'
        (Integral a b, Integral a' b') -> a `eq` a' && b `eq` b'
        _ -> False

data Op = Add
        | Sub
        | Mul
        | Div
        | Pow
        deriving (Eq, Ord)

instance Show Op where
    show = \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Pow -> "^"

instance {-# OVERLAPPABLE #-} Show a => Show (Expr a) where
    show = \case
        BinOp op a b -> show a <> " " <> show op <> " " <> show b
        Const x -> show x
        Sym x -> x
        Sin x -> "sin(" <> show x <> ")"
        Cos x -> "cos(" <> show x <> ")"
        Sqrt x -> "√(" <> show x <> ")"
        Ln x -> "ln(" <> show x <> ")"
        Diff x y -> "d/d_" <> show x <> "(" <> show y <> ")"
        Integral x y -> "∫" <> show y <> " d" <> show x

instance {-# OVERLAPPING #-} Show (Fix Expr) where
    show = foldFix $ \case
        BinOp op a b -> "(" <> a <> " " <> show op <> " " <> b <> ")"
        Const x -> show x
        Sym x -> x
        Sin x -> "sin(" <> x <> ")"
        Cos x -> "cos(" <> x <> ")"
        Sqrt x -> "√(" <> x <> ")"
        Ln x -> "ln(" <> x <> ")"
        Diff x y -> "d/d_" <> x <> "(" <> y <> ")"
        Integral x y -> "∫" <> y <> " d" <> x

instance IsString (Fix Expr) where
    fromString = Fix . Sym

-- WARNING: Careful with negate implementation!!
-- The default implementation makes -1 = 0 - 1
-- Together with some rules this can be disastrous (infinitely)
instance Num (Fix Expr) where
    (+) a b = Fix (BinOp Add a b)
    (-) a b = Fix (BinOp Sub a b)
    (*) a b = Fix (BinOp Mul a b)
    fromInteger = Fix . Const . fromInteger
    negate = Fix . BinOp Mul (-1)
    abs = error "abs"
    signum = error "signum"

instance Fractional (Fix Expr) where
    (/) a b = Fix (BinOp Div a b)
    fromRational = Fix . Const . fromRational

instance Show1 Expr where
    -- ROMES:TODO: Don't ignore precedence?
    liftShowsPrec sp _ d = \case
        BinOp op e1 e2 ->
            sp d e1 . showString (show op) . sp d e2
        Const f -> showString (show f)
        Sym x -> showString x
        Sin x -> showString "sin(" . sp d x . showString ")"
        Cos x -> showString "cos(" . sp d x . showString ")"
        Sqrt x -> showString "√("  . sp d x . showString ")"
        Ln   x -> showString "ln(" . sp d x  . showString ")"
        Diff x y -> showString "d("  . sp d x . showString ")"
        Integral x y -> showString "∫" <> sp d x

symCost :: Expr Cost -> Cost
symCost = \case
    BinOp Pow e1 e2 -> e1 + e2 + 20
    BinOp Div e1 e2 -> e1 + e2 + 5
    BinOp Sub e1 e2 -> e1 + e2 + 5
    BinOp Mul e1 e2 -> e1 + e2 + 4
    BinOp Add e1 e2 -> e1 + e2 + 2
    Sym _ -> 1
    Const _ -> 1
    Sin x -> x + 20
    Cos x -> x + 20
    Sqrt x -> x + 20
    Ln x -> x + 20
    Diff _ x -> x + 10
    Integral _ x -> x + 200

instance Num (PatternAST Expr) where
    (+) a b = NonVariablePattern $ BinOp Add a b
    (-) a b = NonVariablePattern $ BinOp Sub a b
    (*) a b = NonVariablePattern $ BinOp Mul a b
    negate x = NonVariablePattern $ BinOp Mul (-1) x
    fromInteger = NonVariablePattern . Const . fromInteger
    abs = error "abs"
    signum = error "signum"

instance Fractional (PatternAST Expr) where
    (/) a b = NonVariablePattern $ BinOp Div a b
    fromRational = NonVariablePattern . Const . fromRational

pattern PowP a b = NonVariablePattern (BinOp Pow a b)
pattern DiffP a b = NonVariablePattern (Diff a b)
pattern CosP a = NonVariablePattern (Cos a)
pattern SinP a = NonVariablePattern (Sin a)
pattern LnP a = NonVariablePattern (Ln a)

rewrites :: [Rewrite Expr]
rewrites =
    [ "x"+"y" := "y"+"x" -- comm add
    , "x"*"y" := "y"*"x" -- comm mul
    , "x"+("y"+"z") := ("x"+"y")+"z" -- assoc add
    , "x"*("y"*"z") := ("x"*"y")*"z" -- assoc mul

    , "x"-"y" := "x"+(-"y") -- sub cannon
    -- , "x"-"y" := "x"+(-"y") -- TODO div canon

    -- identities
    , "x"+0 := "x"
    , "x"*0 := 0
    , "x"*1 := "x"

    , "x" := "x"+0
    , "x" := "x"*1

    , "a"-"a" := 1 -- cancel sub
    , "a"/"a" := 1 -- cancel div

    , "x"*("y"+"z") := ("x"*"y")+("x"*"z") -- distribute
    , ("x"*"y")+("x"*"z") := "x"+("y"+"z") -- factor

    , "x"*(1/"x") := 1

    -- , PowP "a" "b"*PowP "a" "c" := PowP "a" ("b" + "c") -- pow mul
    -- , PowP "a" 0 := 1
    -- , PowP "a" 1 := "a"
    -- , PowP "a" 2 := "a"*"a"
    -- , PowP "a" (-1) := 1/"a"

    -- -- for test2
    -- , "a"+(-"a") := 0
    -- , "a"-"b" := "a"+(-"b")
    -- -- , -("a"+"b") := -"a"-"b"
    -- , 2*"a" := "a"+"a"

    -- , "a"*(1/"b") := "a"/"b"

    -- , DiffP "x" "x" := 1
    -- , DiffP "x" "y" := 0

    -- , DiffP "x" ("a" + "b") := DiffP "x" "a" + DiffP "x" "b"
    -- , DiffP "x" ("a" * "b") := ("a"*DiffP "x" "b") + ("b"*DiffP "x" "a")

    -- , DiffP "x" (SinP "x") := CosP "x"
    -- , DiffP "x" (CosP "x") := - SinP "x"

    -- , DiffP "x" (LnP "x") := 1/"x"

    -- ...
    ]

rewrites2 :: [Rewrite Expr]
rewrites2 =
    [ -- "x"*("y"+"z") := ("x"*"y")+("x"*"z") -- distribute
    -- , "x"-"y" := "x"+((-1)*"y")
    -- , (-1)*(-1) := 1
    -- , "x"*2 := "x" + "x"
    -- , ("x" + "y") + "z" := "x" + ("y" + "z")
    -- , "a" - "a" := 0
    -- , (-0) := 0
    -- , "x" + 0 := "x" -- id add
    -- , "x" * 1 := "x" -- id mul
    -- , "x"+"y" := "y"+"x" -- comm add
    -- , "x"*"y" := "y"*"x" -- comm mul
    -- "x"+("y"+"z") := ("x"+"y")+"z" -- assoc add
    -- , "x"*("y"*"z") := ("x"*"y")*"z" -- assoc mul
    ]

rewrite :: Fix Expr -> Fix Expr
rewrite e = fst $ equalitySaturation e rewrites symCost

rewrite2 :: Fix Expr -> Fix Expr
rewrite2 e = fst $ equalitySaturation e rewrites2 symCost

symTests :: TestTree
symTests = testGroup "Symbolic"
    [ testCase "1" $
        rewrite (("a"*2)/2) @?= "a"

    , testCase "singleton variable" $
        fst (equalitySaturation (1 + "a") ["x":=0] symCost) @?= 0

    -- , testCase "2" $
    --     rewrite (1 + ("a" - ("a"*(2-1)))) @?= 1

    -- , testCase "d1" $
    --     rewrite (Fix $ Diff "a" "a") @?= 1

    -- , testCase "d2" $
    --     rewrite (Fix $ Diff "a" "b") @?= 0

    -- , testCase "d3" $
    --     rewrite (Fix $ Diff "x" (1 + 2*"x")) @?= 2

    ]
