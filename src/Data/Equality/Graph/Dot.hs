{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Equality.Graph.Dot
    ( module Data.Equality.Graph.Dot
    , writeDotFile
    )
    where

import Control.Monad

import Data.Text.Lazy (Text, pack)

import qualified Data.Set as S
import qualified Data.IntMap as IM

import Data.GraphViz.Commands.IO
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Attributes (style, dotted, textLabel)
import Data.GraphViz.Attributes.Complete

import Data.Equality.Graph
import Data.Equality.Graph.Internal

txt :: Show a => a -> Text
txt = pack . show

writeDemo :: (Language language, Show (ENode language)) => EGraph analysis language -> IO ()
writeDemo = writeDotFile "demo.gv" . toDotGraph

toDotGraph :: (Language language, Show (ENode language)) => EGraph analysis language -> DotGraph Text
toDotGraph eg = digraph (Str "egraph") $ do

    graphAttrs [Compound True, ClusterRank Local]

    forM_ (IM.toList $ classes eg) $ \(class_id, EClass _ nodes _ _) ->

        subgraph (Str ("cluster_" <> txt class_id)) $ do
            graphAttrs [style dotted]
            forM_ (zip (S.toList nodes) [0..]) $ \(n, i) -> do
                let n' = canonicalize n eg
                node (txt class_id <> "." <> txt (find i eg)) [textLabel (txt n')]

    forM_ (IM.toList $ classes eg) $ \(class_id, EClass _ nodes _ _) -> do

        forM_ (zip (S.toList nodes) [0..]) $ \(n, i_in_class) -> do

            let n' = canonicalize n eg
            let i_in_class' = find i_in_class eg

            forM_ (zip (children n') [(0 :: Integer)..]) $ \(child, arg_i) -> do
                -- TODO: On anchors and labels...?
                let child_leader = find child eg
                if child_leader == class_id
                   then edge (txt class_id <> "." <> txt i_in_class') (txt class_id <> "." <> txt i_in_class') [textLabel (txt arg_i)] -- LHead ("cluster_" <> txt class_id),
                   else edge (txt class_id <> "." <> txt i_in_class') (txt child <> ".0") [LHead ("cluster_" <> txt child_leader), textLabel (txt arg_i)]
