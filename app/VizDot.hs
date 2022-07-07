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

import Data.GraphViz.Commands.IO
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Attributes (style, dotted, textLabel)
import Data.GraphViz.Attributes.Complete

-- TODO: Move modules to Equality.Graph.Node, Equality.Saturation, etc...
import EqualitySaturation
import EGraph.ENode
import EGraph.EClass
import EGraph
import EMatching
import Database
import Sym

graph4 :: EGraph ExprF
graph4 = equalitySaturation @ExprF @Expr (("a" + 0) * ("b" + 0)) ["~x"+0 := "~x"]

graph3 :: EGraph ExprF
graph3 = snd $ runEGS emptyEGraph $ do
    reprExpr ("a" + 0 + "b" + 0)

graph2 :: EGraph ExprF
graph2 = snd $ runEGS emptyEGraph $ do
    id1 <- reprExpr ("a" + 0)
    id2 <- reprExpr "a"
    merge id1 id2
    rebuild

txt = pack . show

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

toDotGraph :: (Foldable f, Show (ENode f)) => EGraph f -> DotGraph Text
toDotGraph eg = digraph (Str "egraph") $ do

    graphAttrs [Compound True, ClusterRank Local]

    forM_ (IM.toList $ classes eg) $ \(class_id, EClass _ nodes parents) ->

        subgraph (Str ("cluster_" <> txt class_id)) $ do
            graphAttrs [style dotted]
            forM_ (zip (S.toList nodes) [0..]) $ \(n, i) -> do
                node (txt class_id <> "." <> txt i) [textLabel (txt n)]

    forM_ (IM.toList $ classes eg) $ \(class_id, EClass _ nodes parents) -> do

        forM_ (zip (S.toList nodes) [0..]) $ \(n, i_in_class) -> do

            forM_ (zip (children n) [0..]) $ \(child, arg_i) -> do
                -- TODO: On anchors and labels...?
                let child_leader = find child eg
                if child_leader == class_id
                   then edge (txt class_id <> "." <> txt i_in_class) (txt class_id <> "." <> txt i_in_class) [textLabel (txt arg_i)] -- LHead ("cluster_" <> txt class_id), 
                   else edge (txt class_id <> "." <> txt i_in_class) (txt child <> ".0") [LHead ("cluster_" <> txt child_leader), textLabel (txt arg_i)]
    

main = do
    print graph4
    writeDotFile "egraph.gv" (toDotGraph graph4)

