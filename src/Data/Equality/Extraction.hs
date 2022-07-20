{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Equality.Extraction where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.IntMap as IM

import Control.Monad.State

import Data.Fix

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

type Extraction lang = State (ClassIdMap (Cost, Fix lang))

-- | Simple cost function: the deeper the expression, the bigger the cost
depthCost :: Language l => CostFunction l
depthCost = (+1) . sum . toList

runExtraction :: Extraction lang a -> a
runExtraction = flip evalState mempty

-- | Extract the best expression (@Fix lang@) from an equivalence class, and
-- necessarily all the best sub-expressions from children equilalence classes
--
-- Receives a class id, a cost function, and an e-graph
extractBest :: Language lang
            => EGraph lang -> CostFunction lang -> ClassId -> Fix lang
extractBest g cost (flip find g -> i) = runExtraction $ do

    -- Use `egg`s strategy of find costs for all possible classes and then just
    -- picking up the best from the target e-class.  In practice this shouldn't
    -- find the cost of unused nodes because the "topmost" e-class will be the
    -- target, and all sub-classes must be calculated?

    findCosts g cost

    findBest i >>= \case
        Just (_,n) -> return n
        Nothing    -> error $ "Couldn't find a best node for e-class " <> show i

-- | Find the lowest cost of all e-classes in an e-graph in an extraction
findCosts :: forall lang. Language lang
          => EGraph lang -> CostFunction lang -> Extraction lang ()
findCosts g@EGraph{..} cost = do

    modified <- forM (IM.toList classes) $ \(i, eclass) -> do
        pass <- makePass eclass 
        currentCost <- gets (IM.lookup i)
        case (currentCost, pass) of
            (Nothing, Just new) -> do
                modify (IM.insert i new)
                return True -- modified
            (Just old, Just new)
              | fst new < fst old -> do
                modify (IM.insert i new)
                return True -- modified
            _ -> return False -- not modified

    -- If any class was modified, loop
    if or modified
        then findCosts g cost
        else -- otherwise, finish with debug warnings
            forM_ (IM.toList classes) $ \(i, _) -> do
                gets (IM.lookup i) >>= \case
                    Nothing -> error $ "Faild to compute cost for e-class " <> show i
                    Just _  -> return ()

    where
        -- Get lowest cost and corresponding node of an e-class if possible
        makePass :: EClass lang -> Extraction lang (Maybe (Cost, Fix lang))
        makePass (EClass _ (S.toList -> nodes) _ _) = do
            costs <- catMaybes <$> traverse (nodeTotalCost g cost) nodes
            return (getBest . L.sortBy (\(a,_) (b,_) -> compare a b) $ costs)
          where
            getBest []    = Nothing -- No costs exist for this class, fail to assign a cost
            getBest (x:_) = Just x  -- Cost is the lowest found (list is sorted)

-- | Get the total cost of a node in an e-graph if possible at this stage of
-- the extraction
--
-- For a node to have a cost, all its (canonical) sub-classes have a cost and
-- an associated better expression. We return the constructed best expression
-- with its cost
nodeTotalCost :: Traversable lang => EGraph lang -> CostFunction lang -> ENode lang -> Extraction lang (Maybe (Cost, Fix lang))
nodeTotalCost g cost (Node n) = get >>= \m -> return $ do
    expr <- Fix <$> traverse (fmap snd . (`IM.lookup` m) . flip find g) n
    return (foldFix cost expr, expr)

-- | Find the current best node and its cost in an equivalence class given only the class and the current extraction
-- This is not necessarily the best node in the e-graph, only the best in the current extraction state
--
-- TODO: doesn't really need state here, just read
findBest :: ClassId -> Extraction lang (Maybe (Cost, Fix lang))
findBest i = gets (IM.lookup i)

-- Remove recursive e-nodes from e-class @x@
remove :: Language lang => ClassId -> EGraph lang -> EGraph lang
remove i g = snd $ runEGS g $
    modifyClasses $ flip IM.update i $ \ec@(EClass {eClassNodes = nodes }) ->
        Just ec { eClassNodes = S.filter (not . toRemove) nodes }
            where
                toRemove :: Language lang => ENode lang -> Bool
                toRemove (children -> c) | i `elem` c = True
                toRemove _ = False

