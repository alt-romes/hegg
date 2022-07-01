{-# LANGUAGE LambdaCase #-}
module Sym where

import EGraph

data Expr = Sym String
          | Int Integer
          | BinOp Op Expr Expr
          deriving Show

data Op = Add
        | Sub
        | Mul
        deriving Show

instance Num Expr where
    (+) = BinOp Add
    (-) = BinOp Sub
    (*) = BinOp Mul
    fromInteger = Int
    abs = error "abs"
    signum = error "signum"

reprExpr :: Expr -> EGS String Int ClassId
reprExpr e = do
    i <- getSize
    case e of
      Sym x -> add (ENode i x [])
      Int int -> add (ENode i (show int) [])
      BinOp op e1 e2 -> do
          e1id <- reprExpr e1
          e2id <- reprExpr e2
          add (ENode i (show op) [e1id, e2id])

