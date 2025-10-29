{-# LANGUAGE OverloadedStrings #-}

module VizDot where

import Data.Equality.Graph
import Data.Equality.Graph.Dot
import Data.Equality.Saturation (equalitySaturation)
import SimpleSym

eGraphSymExpr :: EGraph (Maybe Double) SymExpr
eGraphSymExpr = snd $ equalitySaturation e1 rewrites SimpleSym.cost

displaySymExpr :: ENode SymExpr -> Text
displaySymExpr (Node e) = case e of
    Const x -> txt x
    Symbol v -> txt v
    _ :+: _ -> "+"
    _ :*: _ -> "*"
    _ :/: _ -> "/"

simpleSymViz :: DotGraph Text
simpleSymViz = toDotGraph' (Just txt) displaySymExpr eGraphSymExpr

visualizeSaturatedEGraph :: IO ()
visualizeSaturatedEGraph =
    writeDotFile "SimpleSym.dot" $
        toDotGraph' (Just txt) displaySymExpr eGraphSymExpr