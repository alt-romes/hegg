{-# LANGUAGE OverloadedStrings #-}
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

import EGraph.ENode
import EGraph.EClass
import EGraph
import Sym

txt = pack . show

graph1 :: EGraph ExprF
graph1 = snd $ runEGS emptyEGraph $ reprExpr (("a"*2)/2+0) >> reprExpr (2/2) >>= \idiv -> reprExpr 1 >>= \i1 -> reprExpr ("a"*1) >>= \imul1 -> reprExpr "a" >>= \ia -> reprExpr ("a"*(2/2)) >>= \i -> merge imul1 ia >> merge 3 i >> merge i1 idiv >> merge 3 5 >> rebuild

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
    

main = writeDotFile "egraph.gv" (toDotGraph graph1)
