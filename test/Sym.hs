{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Sym where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.String

import Data.Functor.Classes
import Data.Functor.Foldable.TH
import Data.Functor.Foldable

import Data.Equality.Graph
import Data.Equality.Matching
import Data.Equality.Saturation

data Expr = Sym String
          | Integer Integer
          | Rational Rational
          | BinOp Op Expr Expr
          deriving (Show, Eq, Ord)

data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Eq, Ord)

instance Show Op where
    show = \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"

makeBaseFunctor ''Expr

deriving instance Eq a => Eq   (ExprF a)
deriving instance Ord a => Ord  (ExprF a)

instance Show a => Show (ExprF a) where
    show = \case
        BinOpF op a b -> show a <> " " <> show op <> " " <> show b
        RationalF x -> show x
        IntegerF x -> show x
        SymF x -> x

instance IsString Expr where
    fromString = Sym

instance IsString (Fix ExprF) where
    fromString = Fix . SymF

instance Num Expr where
    (+) = BinOp Add
    (-) = BinOp Sub
    (*) = BinOp Mul
    fromInteger = Integer
    abs = error "abs"
    signum = error "signum"

instance Num (Fix ExprF) where
    (+) a b = Fix (BinOpF Add a b)
    (-) a b = Fix (BinOpF Sub a b)
    (*) a b = Fix (BinOpF Mul a b)
    fromInteger = Fix . IntegerF
    abs = error "abs"
    signum = error "signum"

instance Fractional Expr where
    (/) = BinOp Div
    fromRational = Rational

instance Fractional (Fix ExprF) where
    (/) a b = Fix (BinOpF Div a b)
    fromRational = Fix . RationalF

instance ERepr Expr ExprF where
    represent = cata go
        where
            -- ROMES:TODO: Could simplify with traverse somehow?
            go :: ExprF (EGS ExprF ClassId) -> EGS ExprF ClassId
            go e = case e of
              BinOpF op e1 e2 -> do
                  e1id <- e1
                  e2id <- e2
                  add (BinOpF op e1id e2id)
              SymF x ->
                  add (SymF x)
              IntegerF i ->
                  add (IntegerF i)
              RationalF f ->
                  add (RationalF f)

    extract = error "extract not yet implemented"

instance Show1 ExprF where
    -- ROMES:TODO: Don't ignore precedence?
    liftShowsPrec sp _ d = \case
        BinOpF op e1 e2 ->
            sp d e1 . showString (show op) . sp d e2
        SymF x -> showString x
        IntegerF i -> showString (show i)
        RationalF f -> showString (show f)

reprExpr :: Expr -> EGS ExprF ClassId
reprExpr = represent

symCost :: ExprF Cost -> Cost
symCost = \case
    BinOpF Div e1 e2 -> e1 + e2 + 3
    BinOpF _ e1 e2 -> e1 + e2 + 2
    SymF _ -> 1
    IntegerF _ -> 1
    RationalF _ -> 1

instance Num (PatternAST ExprF) where
    (+) a b = NonVariablePattern $ BinOpF Add a b
    (-) a b = NonVariablePattern $ BinOpF Sub a b
    (*) a b = NonVariablePattern $ BinOpF Mul a b
    fromInteger = NonVariablePattern . IntegerF
    abs = error "abs"
    signum = error "signum"

instance Fractional (PatternAST ExprF) where
    (/) a b = NonVariablePattern $ BinOpF Div a b
    fromRational = NonVariablePattern . RationalF

symTests :: TestTree
symTests = testGroup "Symbolic"
    [
    ]
