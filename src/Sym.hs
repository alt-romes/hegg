{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Sym where

import Data.String

import Data.Functor.Foldable.TH
import Data.Functor.Foldable

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
        deriving (Show, Eq, Ord)

makeBaseFunctor ''Expr

deriving instance Eq   (ExprF ClassId)
deriving instance Ord  (ExprF ClassId)
deriving instance Show (ExprF ClassId)

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

