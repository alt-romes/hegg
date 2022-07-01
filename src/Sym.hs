{-# LANGUAGE LambdaCase #-}
module Sym where

import Data.String

import EGraph

data Expr = Sym String
          | Integer Integer
          | Rational Rational
          | BinOp Op Expr Expr
          deriving Show

data Op = Add
        | Sub
        | Mul
        | Div
        deriving Show

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

reprExpr :: Expr -> EGS String Int ClassId
reprExpr e = do
    i <- getSize
    case e of
      Sym x -> add (ENode i x [])
      Integer int -> add (ENode i (show int) [])
      Rational f -> add (ENode i (show f) [])
      BinOp op e1 e2 -> do
          e1id <- reprExpr e1
          e2id <- reprExpr e2
          add (ENode i (show op) [e1id, e2id])

