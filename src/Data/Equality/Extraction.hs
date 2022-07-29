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

import Prelude hiding (Maybe(..))
import Data.Foldable (toList)
import qualified Data.Maybe as MB

import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM

import Control.Monad.State.Strict

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

-- | Instance Ord for Maybe in which Just is always considered smaller than Nothing
newtype NMaybe a = NM (MB.Maybe a)
  deriving (Eq, Functor, Applicative, Monad)

pattern Nothing :: NMaybe a
pattern Nothing = NM MB.Nothing

pattern Just :: a -> NMaybe a
pattern Just a = NM (MB.Just a)

{-# COMPLETE Nothing, Just #-}

instance Ord a => Ord (NMaybe a) where
  compare (Nothing) (Nothing) = EQ
  compare (Just _) (Nothing) = LT
  compare (Nothing) (Just _) = GT
  compare (Just x) (Just y) = compare x y

-- | 'Cost' is simply an integer
type Cost = Int

newtype CostWithExpr lang = CostWithExpr { unCWE :: (Cost, Fix lang) }

instance Eq (CostWithExpr lang) where
  (==) (CostWithExpr (a,_)) (CostWithExpr (b,_)) = a == b
  {-# INLINE (==) #-}

instance Ord (CostWithExpr lang) where
  compare (CostWithExpr (a,_)) (CostWithExpr (b,_)) = a `compare` b
  {-# INLINE compare #-}

type Extraction lang = State (ClassIdMap (CostWithExpr lang))

-- | Simple cost function: the deeper the expression, the bigger the cost
depthCost :: Language l => CostFunction l
depthCost = (+1) . sum . toList
{-# INLINE depthCost #-}

runExtraction :: Extraction lang a -> ClassIdMap (CostWithExpr lang)
runExtraction = snd . flip runState mempty
{-# INLINE runExtraction #-}

-- | Extract the best expression (@Fix lang@) from an equivalence class, and
-- necessarily all the best sub-expressions from children equilalence classes
--
-- Receives a class id, a cost function, and an e-graph
extractBest :: forall lang. Language lang
            => EGraph lang -> CostFunction lang -> ClassId -> Fix lang
extractBest g@EGraph{..} cost (flip find g -> i) = 

    -- Use `egg`s strategy of find costs for all possible classes and then just
    -- picking up the best from the target e-class.  In practice this shouldn't
    -- find the cost of unused nodes because the "topmost" e-class will be the
    -- target, and all sub-classes must be calculated?
    let extr = runExtraction findCosts

     in case findBest i extr of
        Just (CostWithExpr (_,n)) -> n
        Nothing    -> error $ "Couldn't find a best node for e-class " <> show i

  where
    -- | Find the lowest cost of all e-classes in an e-graph in an extraction
    findCosts :: Extraction lang ()
    findCosts = do

        modified <- sequence $ IM.mapWithKey (\i' eclass -> get >>= \m -> do
            let currentCost = NM (IM.lookup i' m)
            case (currentCost, makePass eclass m) of
                (Nothing, Just new) -> do
                    modify (IM.insert i' new)
                    return True -- modified
                (Just (CostWithExpr old), Just (CostWithExpr new))
                  | fst new < fst old -> do
                    modify (IM.insert i' (CostWithExpr new))
                    return True -- modified
                _ -> return False -- not modified
                                  ) classes

        -- If any class was modified, loop
        if or modified
            then findCosts
            else -- otherwise, finish with debug warnings
                -- forM_ (IM.toList classes) $ \(i, _) -> do
                --     gets (IM.lookup i) >>= \case
                --         Nothing -> error $ "Faild to compute cost for e-class " <> show i
                --         Just _  -> return ()
                return ()

        where
            -- Get lowest cost and corresponding node of an e-class if possible
            makePass :: EClass lang -> ClassIdMap (CostWithExpr lang) -> NMaybe (CostWithExpr lang)
            makePass (EClass _ nodes _ _) m =
                let costs = S.map (nodeTotalCost m) nodes
                 in if S.null costs
                      then Nothing
                      else S.findMin costs
            {-# INLINE makePass #-}
    {-# SCC findCosts #-}

    -- | Get the total cost of a node in an e-graph if possible at this stage of
    -- the extraction
    --
    -- For a node to have a cost, all its (canonical) sub-classes have a cost and
    -- an associated better expression. We return the constructed best expression
    -- with its cost
    nodeTotalCost :: Traversable lang => ClassIdMap (CostWithExpr lang) -> ENode lang -> NMaybe (CostWithExpr lang)
    nodeTotalCost m (Node n) = do
        expr <- Fix <$> traverse (NM . fmap @MB.Maybe (snd . unCWE) . (`IM.lookup` m) . flip find g) n
        return $ CostWithExpr (cata cost expr, expr)
    {-# INLINE nodeTotalCost #-}

{-# SCC extractBest #-}

-- | Find the current best node and its cost in an equivalence class given only the class and the current extraction
-- This is not necessarily the best node in the e-graph, only the best in the current extraction state
findBest :: ClassId -> ClassIdMap (CostWithExpr lang) -> NMaybe (CostWithExpr lang)
findBest i = NM . IM.lookup i
{-# INLINE findBest #-}

