{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-|
   Given an e-graph representing expressions of our language, we might want to
   extract, out of all expressions represented by some equivalence class, /the best/
   expression (according to a 'CostFunction') represented by that class

   The function 'extractBest' allows us to do exactly that: get the best
   expression represented in an e-class of an e-graph given a 'CostFunction'
 -}
module Data.Equality.Extraction
  (
  -- * Extraction
    extractBest

  -- * Cost
  , CostFunction
  , depthCost
  ) where

import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM

import Data.Equality.Graph.Internal (EGraph(classes))
import Data.Equality.Utils
import Data.Equality.Graph

-- vvvv and necessarily all the best sub-expressions from children equilalence classes

-- | Extract the /best/ expression from an equivalence class according to a
-- 'CostFunction'
--
-- @
-- (i, egr) = ...
--    i <- represent expr
--            ...
--
-- bestExpr = extractBest egr 'depthCost' i
-- @
--
-- For a real example you might want to check out the source code of 'Data.Equality.Saturation.equalitySaturation''
extractBest :: forall anl lang cost
             . (Language lang, Ord cost)
            => EGraph anl lang        -- ^ The e-graph out of which we are extracting an expression
            -> CostFunction lang cost -- ^ The cost function to define /best/
            -> ClassId                -- ^ The e-class from which we'll extract the expression
            -> Fix lang               -- ^ The resulting /best/ expression, in its fixed point form.
extractBest egr cost (flip find egr -> i) = 

    -- Use `egg`s strategy of find costs for all possible classes and then just
    -- picking up the best from the target e-class.  In practice this shouldn't
    -- find the cost of unused nodes because the "topmost" e-class will be the
    -- target, and all sub-classes must be calculated?
    let allCosts = findCosts (classes egr) mempty

     in case findBest i allCosts of
        Just (CostWithExpr (_,n)) -> n
        Nothing    -> error $ "Couldn't find a best node for e-class " <> show i

  where

    -- | Find the lowest cost of all e-classes in an e-graph in an extraction
    findCosts :: ClassIdMap (EClass anl lang) -> ClassIdMap (CostWithExpr lang cost) -> ClassIdMap (CostWithExpr lang cost)
    findCosts eclasses current =

      let (modified, updated) = IM.foldlWithKey f (False, current) eclasses

          {-# INLINE f #-}
          f :: (Bool, ClassIdMap (CostWithExpr lang cost)) -> Int -> EClass anl lang -> (Bool, ClassIdMap (CostWithExpr lang cost))
          f acc@(_, beingUpdated) i' EClass{eClassNodes = nodes} =
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

    -- | Get the total cost of a node in an e-graph if possible at this stage of
    -- the extraction
    --
    -- For a node to have a cost, all its (canonical) sub-classes have a cost and
    -- an associated better expression. We return the constructed best expression
    -- with its cost
    nodeTotalCost :: Traversable lang => ClassIdMap (CostWithExpr lang cost) -> ENode lang -> Maybe (CostWithExpr lang cost)
    nodeTotalCost m (Node n) = do
        expr <- traverse ((`IM.lookup` m) . flip find egr) n
        return $ CostWithExpr (cost (fst . unCWE <$> expr), Fix $ snd . unCWE <$> expr)
    {-# INLINE nodeTotalCost #-}
{-# INLINABLE extractBest #-}

-- | A cost function is used to attribute a cost to representations in the
-- e-graph and to extract the best one.
--
-- The cost function is polymorphic over the type used for the cost, however
-- @cost@ must instance 'Ord' in order for the defined 'CostFunction' to
-- fulfill its purpose. That's why we have an @Ord cost@ constraint in
-- 'Data.Equality.Saturation.equalitySaturation' and 'extractBest'
--
-- === Example
-- @
-- symCost :: Expr Int -> Int
-- symCost = \case
--     BinOp Integral e1 e2 -> e1 + e2 + 20000
--     BinOp Diff e1 e2 -> e1 + e2 + 500
--     BinOp x e1 e2 -> e1 + e2 + 3
--     UnOp x e1 -> e1 + 30
--     Sym _ -> 1
--     Const _ -> 1
-- @
type CostFunction l cost = l cost -> cost

-- | Simple cost function: the deeper the expression, the bigger the cost
depthCost :: Language l => CostFunction l Int
depthCost = (+1) . sum
{-# INLINE depthCost #-}

-- | Find the current best node and its cost in an equivalence class given only the class and the current extraction
-- This is not necessarily the best node in the e-graph, only the best in the current extraction state
findBest :: ClassId -> ClassIdMap (CostWithExpr lang a) -> Maybe (CostWithExpr lang a)
findBest = IM.lookup
{-# INLINE findBest #-}

newtype CostWithExpr lang a = CostWithExpr { unCWE :: (a, Fix lang) }

instance Eq a => Eq (CostWithExpr lang a) where
  (==) (CostWithExpr (a,_)) (CostWithExpr (b,_)) = a == b
  {-# INLINE (==) #-}

instance Ord a => Ord (CostWithExpr lang a) where
  compare (CostWithExpr (a,_)) (CostWithExpr (b,_)) = a `compare` b
  {-# INLINE compare #-}

