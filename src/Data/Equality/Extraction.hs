{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Equality.Extraction
  (extractBest, depthCost, CostFunction, Cost) where

import Data.Foldable (toList)

import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM

import Data.Equality.Utils
import Data.Equality.Graph

-- | A cost function is used to attribute a cost to representations in the
-- e-graph and to extract the best one.
--
-- === Example
-- @
-- symCost :: Expr Cost -> Cost
-- symCost = \case
--     BinOp Integral e1 e2 -> e1 + e2 + 20000
--     BinOp Diff e1 e2 -> e1 + e2 + 500
--     BinOp x e1 e2 -> e1 + e2 + 3
--     UnOp Negate e1 -> e1 + 5
--     UnOp x e1 -> e1 + 30
--     Sym _ -> 1
--     Const _ -> 1
-- @
type CostFunction l = l Cost -> Cost

-- | 'Cost' is simply an integer
type Cost = Int

newtype CostWithExpr lang = CostWithExpr { unCWE :: (Cost, Fix lang) }

instance Eq (CostWithExpr lang) where
  (==) (CostWithExpr (a,_)) (CostWithExpr (b,_)) = a == b
  {-# INLINE (==) #-}

instance Ord (CostWithExpr lang) where
  compare (CostWithExpr (a,_)) (CostWithExpr (b,_)) = a `compare` b
  {-# INLINE compare #-}

-- | Simple cost function: the deeper the expression, the bigger the cost
depthCost :: Language l => CostFunction l
depthCost = (+1) . sum . toList
{-# INLINE depthCost #-}

-- | Extract the best expression (@Fix lang@) from an equivalence class, and
-- necessarily all the best sub-expressions from children equilalence classes
--
-- Receives a class id, a cost function, and an e-graph
extractBest :: forall lang. Language lang
            => EGraph lang -> CostFunction lang -> ClassId -> Fix lang
extractBest g@EGraph{classes = eclasses'} cost (flip find g -> i) = 

    -- Use `egg`s strategy of find costs for all possible classes and then just
    -- picking up the best from the target e-class.  In practice this shouldn't
    -- find the cost of unused nodes because the "topmost" e-class will be the
    -- target, and all sub-classes must be calculated?
    let allCosts = findCosts eclasses' mempty

     in case findBest i allCosts of
        Just (CostWithExpr (_,n)) -> n
        Nothing    -> error $ "Couldn't find a best node for e-class " <> show i

  where

    -- | Find the lowest cost of all e-classes in an e-graph in an extraction
    findCosts :: ClassIdMap (EClass lang) -> ClassIdMap (CostWithExpr lang) -> ClassIdMap (CostWithExpr lang)
    findCosts eclasses current =

      let (modified, updated) = IM.foldlWithKey f (False, current) eclasses

          {-# INLINE f #-}
          f :: (Bool, ClassIdMap (CostWithExpr lang)) -> Int -> EClass lang -> (Bool, ClassIdMap (CostWithExpr lang))
          f = \acc@(_, beingUpdated) i' (EClass _ nodes _ _) ->

                let
                    currentCost = IM.lookup i' beingUpdated

                    newCost = S.foldl' (\c n -> case (c, nodeTotalCost beingUpdated n) of
                                                  (Nothing, Nothing) -> Nothing
                                                  (Nothing, Just nc) -> Just nc
                                                  (Just oc, Nothing) -> Just oc
                                                  (Just oc, Just nc) -> Just (oc `min` nc)
                                       ) Nothing nodes
                    -- Current cost + get lowest cost and corresponding node of an e-class if possible
                 in case (currentCost, newCost) of

                    (Nothing, Just new) -> (True, IM.insert i' new beingUpdated)

                    (Just (CostWithExpr old), Just (CostWithExpr new))
                      | fst new < fst old -> (True, IM.insert i' (CostWithExpr new) beingUpdated)

                    _ -> acc

        -- If any class was modified, loop
       in if modified
            then findCosts eclasses updated
            else updated

    {-# SCC findCosts #-}

    -- | Get the total cost of a node in an e-graph if possible at this stage of
    -- the extraction
    --
    -- For a node to have a cost, all its (canonical) sub-classes have a cost and
    -- an associated better expression. We return the constructed best expression
    -- with its cost
    nodeTotalCost :: Traversable lang => ClassIdMap (CostWithExpr lang) -> ENode lang -> Maybe (CostWithExpr lang)
    nodeTotalCost m (Node n) = do
        expr <- traverse ((`IM.lookup` m) . flip find g) n
        return $ CostWithExpr (cost ((fst . unCWE) <$> expr), (Fix $ (snd . unCWE) <$> expr))
    {-# INLINE nodeTotalCost #-}
    -- {-# SCC nodeTotalCost #-}
{-# SCC extractBest #-}



-- | Find the current best node and its cost in an equivalence class given only the class and the current extraction
-- This is not necessarily the best node in the e-graph, only the best in the current extraction state
findBest :: ClassId -> ClassIdMap (CostWithExpr lang) -> Maybe (CostWithExpr lang)
findBest i = IM.lookup i
{-# INLINE findBest #-}

