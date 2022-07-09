{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Invariants where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Control.Monad

import qualified Data.IntMap as IM
import qualified Data.Map as M

import Data.Equality.Graph
import Data.Equality.Saturation
import Sym
import Dot

-- The equivalence relation over e-nodes must be closed over congruence
-- congruenceInvariant :: Testable m (EGraph lang) => Property m


-- The hashcons 𝐻  must map all canonical e-nodes to their e-class ids
--
-- Note: the e-graph argument must have been rebuilt -- checking the property
-- when invariants are broken for sure doesn't make much sense
hashConsInvariant :: forall s. (Show (ENode s), Ord (ENode s), Functor s, Foldable s) => EGraph s -> Bool
hashConsInvariant eg@(EGraph {..}) =
    all f (IM.toList classes)
    where
        -- e-node 𝑛 ∈ 𝑀 [𝑎] ⇐⇒ 𝐻 [canonicalize(𝑛)] = find(𝑎)
        f :: (Ord (ENode s), Functor s) => (ClassId, EClass s) -> Bool
        f (i, EClass _ nodes _) = all g nodes
            where
                g :: (Ord (ENode s), Functor s) => ENode s -> Bool
                g en = case M.lookup (canonicalize en eg) memo of
                    Nothing -> error "how can we not find canonical thing in map? :)" -- False
                    Just i' -> i' == find i eg 

-- ROMES:TODO: Property: Extract expression after equality saturation is always better or equal to the original expression


hciSym :: EGraph Expr -> Bool
hciSym = hashConsInvariant

instance Arbitrary (EGraph Expr) where
    arbitrary = sized $ \n -> do
        exps <- forM [0..n] $ const arbitrary
        -- rws :: [Rewrite Expr] <- forM [0..n] $ const arbitrary
        (ids, eg) <- return $ runEGS emptyEGraph $
            mapM represent exps
        ids1 <- sublistOf ids
        ids2 <- sublistOf ids
        return $ snd $ runEGS eg $ do
            forM_ (zip ids1 ids2) $ \(a,b) -> do
                merge a b
            rebuild

instance Arbitrary Op where
    arbitrary = oneof [ return Add
                      , return Sub
                      , return Mul
                      , return Div ]

instance Arbitrary (Fix Expr) where
    arbitrary = sized (fmap Fix . expr')
        where
            expr' 0 = oneof [ Sym <$> arbitrary
                            , Const . fromInteger <$> arbitrary
                            ]
            expr' n
              | n > 0 = BinOp <$> arbitrary <*> subexpr <*> subexpr
              where 
                subexpr = Fix <$> expr' (n `div` 2)
            expr' _ = error "size is negative?"

invariants :: TestTree
invariants = testGroup "Invariants"
  [ QC.testProperty "Hash Cons Invariant" hciSym
  ]

