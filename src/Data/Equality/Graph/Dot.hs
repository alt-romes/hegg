{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module providing visualization of e-graph in [DOT](https://graphviz.org/doc/info/lang.html#subgraphs-and-clusters) format.

  === __Example Usage__

  @
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
  @
-}
module Data.Equality.Graph.Dot (
  -- * Generate and writing DOT Graph from an e-graph
  toDotGraph,
  toDotGraph',

  -- ** GraphViz re-exports
  writeDotFile,
  DotGraph (),

  -- ** Text re-exports
  Text (),

  -- ** Helper function
  txt,
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

{- | Visualize an e-graph as a GraphViz Dot file. The analysis are display is not shown and the e-node labels are rendered using the default `Show` instance.

@toDotGraph = toDotGraph' Nothing txt@
-}
toDotGraph ::
  ( Language l
  , Show (ENode l)
  , Show analysis
  ) =>
  EGraph analysis l ->
  DotGraph Text
toDotGraph = toDotGraph' Nothing txt

-- | Visualize an e-graph as a GraphViz Dot file with additional control on how analysis and e-node labeled are rendered as `Text`.
toDotGraph' ::
  (Language l) =>
  -- | an optional function to set the display of `domain` for `Analysis` as Text. Use `Nothing` to hide the analysis.
  Maybe (anl -> Text) ->
  {- | a function specifying how e-node labels are generated as `Text`.

  (e.g. one might want @Node (eID1 :+: eID2)@ to be rendered as @"+"@ ignoring the e-class IDs)
  -}
  (ENode l -> Text) ->
  -- | the e-graph to visualize
  EGraph anl l ->
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
      , OutputOrder EdgesFirst
      , styles [dashed, rounded, filled]
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

-- | Each e-node with text `t` of arity `n` is drawn as a table with n cols and 2 rows. The first row contain a single cell spanning n cols and displaying the  e-node text `t`. The second row contains "empty" cells having zero heights functioning as anchor point (used in each edge's TailPort attribute). This eliminates the need to use edge labels as a mean to understand positional arguments.
hiddenTable ::
  -- | text used to display the e-node
  Text ->
  -- | arity of the enode
  Int ->
  HTML.Table
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
