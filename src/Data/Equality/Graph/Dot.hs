{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Equality.Graph.Dot (
  module Data.Equality.Graph.Dot,
  Text,
  DotGraph,
  writeDotFile,
)
where

import Control.Monad

import Data.Text.Lazy (Text, pack)

import qualified Data.IntMap as IM
import qualified Data.Set as S

import Data.GraphViz.Commands.IO
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic

import Data.GraphViz hiding (DotGraph)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Attributes.HTML (Table (..))
import qualified Data.GraphViz.Attributes.HTML as HTML

import Data.Equality.Graph
import Data.Equality.Graph.Internal

txt :: (Show a) => a -> Text
txt = pack . show

writeDemo :: (Language language, Show (ENode language), Show analysis) => EGraph analysis language -> IO ()
writeDemo = writeDotFile "demo.gv" . toDotGraph

{- | Visualize an e-graph as a GraphViz Dot file. The analysis are display is not shown and the e-node label is displayed using the default `Show` instance.

This is a default case for toDotGraph' where
- `anlText` is `Nothing`
- `eNodeText` is `txt`
-}
toDotGraph :: (Language language, Show (ENode language), Show analysis) => EGraph analysis language -> DotGraph Text
toDotGraph = toDotGraph' Nothing txt

{- | Visualize an e-graph as a GraphViz Dot file.

- `anlText`:  an optional function to set the display of `domain` for `Analysis` as Text. Use `Nothing` to disable the analysis display.
- `eNodeText`: a function to set the display of e-nodes of the language as text. One can think of it as the text for the "constructors + its non-recursive arguments".
-}
toDotGraph' ::
  (Language language) =>
  Maybe (anl -> Text) ->
  (ENode language -> Text) ->
  EGraph anl language ->
  DotGraph Text
toDotGraph' anlText eNodeText eg = digraph (Data.GraphViz.Types.Monadic.Str "egraph") $ do
  globallAttrs
  forM_ (IM.toList $ classes eg) $ \(class_id, EClass _ nodes analysis _) -> do
    drawOutEdges eg class_id nodes
    subgraph (Str $ "outercluster_" <> txt class_id) $ do
      subgraph (Str $ "cluster_" <> txt class_id) $ do
        -- add analysis text to e-class if anlText is available
        _ <- case anlText of
          Nothing -> return ()
          Just mkAnlText -> graphAttrs [toLabel $ mkAnlText analysis]
        forM_ (zip (S.toList nodes) [1 ..]) $ \(n, i) -> do
          -- draw e-node (assign Dot node ID, assign label)
          let n' = canonicalize n eg
          node
            (txt class_id <> "." <> txt i)
            [ toLabel $
                HTML.Table $
                  hiddenTable (eNodeText n') (length $ children n')
            , styles [rounded, filled]
            , fillColor White
            , shape BoxShape
            ]
 where
  globallAttrs = do
    graphAttrs
      [ Compound True
      , ClusterRank Local
      , FontSize 9
      , NodeSep 0.05
      , RankSep [0.6]
      , -- , ColorScheme $ Brewer $ BScheme Set3 12
        OutputOrder EdgesFirst
      , styles [dashed, rounded, filled]
      -- , fillColor $ BrewerColor $ BC
      ]
    edgeAttrs
      [ ArrowSize 0.5
      ]
    nodeAttrs
      [ shape PlainText
      , FontName "helvetica"
      ]

sourceLabel :: ClassId -> Int -> Text
sourceLabel classId nodeInClass =
  txt classId <> "." <> txt nodeInClass

-- | For a given e-graph `eg`, draw all the out-going edges for nodes from a e-class with ID `classId`. Idealy we want the edges to point to e-classes as oppose to e-nodes, but because of how GraphViz edges are forces to point edge to a specific e-node. We can simulate the visual by "cliping" the arrows head to the e-class subgraph (using the LHead attribute). This specific e-node can chosen as the first node in the e-class, and there is no need to find the canonical node.
drawOutEdges :: (Language l) => EGraph a l -> Int -> S.Set (ENode l) -> Dot Text
drawOutEdges eg classId eNodes = forM_ (zip (S.toList eNodes) [1 ..]) $
  \(n, i_in_class) -> do
    let n' = canonicalize n eg
    forM_ (zip (children n') [(1 :: Integer) ..]) $
      \(child, arg_i) -> do
        let edgeSource = sourceLabel classId i_in_class
        edge
          edgeSource
          (txt child <> ".1")
          [ LHead ("cluster_" <> txt child)
          , TailPort $ LabelledPort (PN $ txt arg_i) $ Just South
          ]

{- | Each e-node with text `t` of arity `n` is drawn as a table with n cols and 2 rows. The first row contain a single cell spanning n cols and displaying the  e-node text `t`. The second row contains "empty" cells having zero heights functioning as anchor point (used in each edge's TailPort attribute). This eliminates the need to use edge labels as a mean to understand positional arguments.
- `t` text used to display the e-node
- `n` arity of the enode
-}
hiddenTable :: Text -> Int -> HTML.Table
hiddenTable t arity =
  HTML.HTable
    { tableFontAttrs = Nothing
    , tableAttrs = [HTML.CellSpacing 0, HTML.CellPadding 0, HTML.CellBorder 0, HTML.Align HTML.HCenter, HTML.Style HTML.Rounded, HTML.Border 0]
    , tableRows = rs
    }
 where
  rs =
    [ HTML.Cells
        [ HTML.LabelCell
            [ HTML.ColSpan (fromIntegral $ max 1 arity)
            , HTML.Width 30
            , HTML.Height 30
            , HTML.CellPadding 4
            ]
            $ HTML.Text [HTML.Str t]
        ]
    , HTML.Cells mkPortRow
    ]

  mkPortRow = (<$> [1 .. max 1 arity]) $ \i ->
    HTML.LabelCell
      [ HTML.Port $
          PN $
            txt i
      ]
      $ HTML.Text []
