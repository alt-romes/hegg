{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE BangPatterns #-}
module Data.Equality.Extraction
  ( extractBest
  , CostFunction
  , costOnly
  , depthCost
  ) where

import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Equality.Graph.Internal (EGraph(classes))
import Data.Equality.Utils
import Data.Equality.Graph

{-|

Extraction of the /best/ expression from an e-class according to a
'CostFunction'.

Uses a two-phase approach:

1. __Reachability__: walk down from the target class to find all classes
  that could contribute to the extracted expression.
2. __Fixed-point costing__: run the iterative DP only over the reachable
  subset, skipping the (often much larger) set of unreachable classes.

 -}
extractBest :: forall anl lang cost
             . (Language lang, Ord cost)
            => EGraph anl lang
            -> CostFunction anl lang cost
            -> ClassId
            -> Fix lang
extractBest egr cost (flip find egr -> root) =
    let !reachable = reachableFrom root IS.empty
        !reachableCls = IM.restrictKeys cls reachable
        !allCosts = findCosts reachableCls IM.empty
    in case IM.lookup root allCosts of
         Just (CostWithExpr (_, e)) -> e
         Nothing -> error $ "extractBest: couldn't extract e-class " <> show root
  where
    !cls = classes egr

    reachableFrom :: ClassId -> IS.IntSet -> IS.IntSet
    reachableFrom (flip find egr -> cid) !visited
        | IS.member cid visited = visited
        | otherwise = case IM.lookup cid cls of
            Nothing -> visited
            Just EClass{eClassNodes = nodes} ->
                let !visited' = IS.insert cid visited
                in S.foldl' (\v (Node n) -> foldl' (flip reachableFrom) v n)
                            visited' nodes

    -- Fixed-point iteration over a (small) subset of classes
    findCosts :: ClassIdMap (EClass anl lang)
              -> ClassIdMap (CostWithExpr lang cost)
              -> ClassIdMap (CostWithExpr lang cost)
    findCosts eclasses current =
        let (!modified, !updated) = IM.foldlWithKey' f (False, current) eclasses
        in if modified then findCosts eclasses updated else updated
      where
        {-# INLINE f #-}
        f (!changed, !m) i' EClass{eClassNodes = nodes, eClassData = anl} =
            let !newCost = S.foldl' (\c n -> minMaybe c (nodeTotalCost anl m n)) Nothing nodes
            in case (IM.lookup i' m, newCost) of
                 (Nothing, Just new) -> (True, IM.insert i' new m)
                 (Just (CostWithExpr old), Just (CostWithExpr new))
                   | fst new < fst old -> (True, IM.insert i' (CostWithExpr new) m)
                 _ -> (changed, m)

    nodeTotalCost :: anl -> ClassIdMap (CostWithExpr lang cost)
                  -> ENode lang -> Maybe (CostWithExpr lang cost)
    nodeTotalCost anl m (Node n) = do
        expr <- traverse lookupChild n
        return $! CostWithExpr { unCWE = (cost anl (fst <$> expr), Fix { unFix = snd <$> expr }) }
      where
        lookupChild cid = do
            let !cid' = find cid egr
            (!c, !e) <- unCWE <$> IM.lookup cid' m
            let !childAnl = eClassData (cls IM.! cid')
            return ((childAnl, c), e)
    {-# INLINE nodeTotalCost #-}

    minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
    minMaybe (Just a) (Just b) = Just (min a b)
    minMaybe a b = a <|> b
    {-# INLINE minMaybe #-}
{-# INLINABLE extractBest #-}

-- | A cost function attributes a cost to e-node representations in the
-- e-graph.  @cost@ must instance 'Ord'.
type CostFunction anl l cost = anl -> l (anl, cost) -> cost

-- | Lift a cost function that ignores analysis data.
costOnly :: Functor l => (l cost -> cost) -> CostFunction anl l cost
costOnly f _ = f . fmap snd
{-# INLINE costOnly #-}

-- | Simple depth cost.
depthCost :: Language l => CostFunction anl l Int
depthCost _ = (+1) . sum . fmap snd
{-# INLINE depthCost #-}

newtype CostWithExpr lang a = CostWithExpr { unCWE :: (a, Fix lang) }

instance Eq a => Eq (CostWithExpr lang a) where
  (==) (CostWithExpr (a,_)) (CostWithExpr (b,_)) = a == b
  {-# INLINE (==) #-}

instance Ord a => Ord (CostWithExpr lang a) where
  compare (CostWithExpr (a,_)) (CostWithExpr (b,_)) = a `compare` b
  {-# INLINE compare #-}
