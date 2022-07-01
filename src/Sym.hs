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
          deriving Show

data Op = Add
        | Sub
        | Mul
        | Div
        deriving Show

makeBaseFunctor ''Expr

-- instance ENode (ExprF ClassId) where
--     children = \case
--         BinOp _ a b -> [a, b]
--         _ -> []

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
reprExpr = cata go
    where
        go :: ExprF (EGS String Int ClassId) -> EGS String Int ClassId
        go e = do
            i <- getSize
            case e of
              SymF x ->
                  add (ENode i x [])
              IntegerF int ->
                  add (ENode i (show int) [])
              RationalF f ->
                  add (ENode i (show f) [])
              BinOpF op e1 e2 -> do
                  e1id <- e1
                  e2id <- e2
                  i <- getSize
                  add (ENode i (show op) [e1id, e2id])

