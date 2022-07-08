{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Control.Monad

import Data.Text.Lazy (Text, pack)

import qualified Data.Set as S
import qualified Data.IntMap as IM

import Data.Equality.Saturation
import Data.Equality.Graph
import Data.Equality.Matching
import Database
import Sym
import Dot

graph4 :: EGraph ExprF
graph4 = equalitySaturation @Expr @ExprF (("a" + 0) * ("b" + 0)) ["~x"+0 := "~x"]

graph3 :: EGraph ExprF
graph3 = snd $ runEGS emptyEGraph $ do
    reprExpr ("a" + 0 + "b" + 0)

graph2 :: EGraph ExprF
graph2 = snd $ runEGS emptyEGraph $ do
    id1 <- reprExpr ("a" + 0)
    id2 <- reprExpr "a"
    merge id1 id2
    rebuild

graph1 :: EGraph ExprF
graph1 = snd $ runEGS emptyEGraph $ do
    reprExpr (("a"*2)/2+0)
    idiv <- reprExpr (2/2)
    i1 <- reprExpr 1
    imul1 <- reprExpr ("a"*1)
    ia <- reprExpr "a"
    i <- reprExpr ("a"*(2/2))
    -- merge imul1 ia
    -- merge 3 i
    -- merge i1 idiv
    -- merge 3 5
    rebuild

main = do
    writeDotFile "egraph.gv" (toDotGraph graph4)

