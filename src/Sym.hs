{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Sym where

import Data.String

import Data.Functor.Foldable.TH
import Data.Functor.Foldable

import EMatching
import EGraph

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

instance Num Expr where
    (+) = BinOp Add
    (-) = BinOp Sub
    (*) = BinOp Mul
    fromInteger = Integer
    abs = error "abs"
    signum = error "signum"

instance Fractional Expr where
    (/) = BinOp Div
    fromRational = Rational

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

reprExpr :: Expr -> EGS ExprF ClassId
reprExpr = represent

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
