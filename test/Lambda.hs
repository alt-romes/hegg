{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module Lambda where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as S

import Control.Applicative ((<|>))

import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving

import Data.Equality.Graph.Lens
import Data.Equality.Graph.Monad as GM
import Data.Equality.Graph
import Data.Equality.Extraction
import Data.Equality.Analysis
import Data.Equality.Saturation
import Data.Equality.Matching

data Lambda a
    = Bool Bool
    | Num Int
    | Var a
    | Add a a
    | Eq a a
    | App a a
    | Lam a a
    | Let a a a
    | LFix a a
    | If a a a
    | Symbol String
    deriving ( Eq, Ord, Functor
             , Foldable, Traversable
             )

deriveEq1 ''Lambda
deriveOrd1 ''Lambda
deriveShow1 ''Lambda

data Data = Data { free :: S.Set ClassId
                 , constant :: Maybe (Fix Lambda)
                 } deriving Eq

evalL :: EGraph Lambda -> Lambda ClassId -> Maybe (Fix Lambda)
evalL egr = \case
    Bool n -> Just (Fix $ Bool n)
    Num n  -> Just (Fix $ Num n)
    Add a b -> do
        a' <- constant (egr^._class a._data) >>= num
        b' <- constant (egr^._class b._data) >>= num
        return (Fix $ Num $ a' + b')
    Eq  a b -> do
        a' <- constant (egr^._class a._data)
        b' <- constant (egr^._class b._data)
        return (Fix $ Bool $  a' == b')
    _ -> Nothing
  where
    num :: Fix Lambda -> Maybe Int
    num = \case
        Fix (Num i) -> Just i
        _ -> Nothing

instance Analysis Lambda where
    type Domain Lambda = Data

    makeA n egr =
      let
          freeVs = case unNode n of
            Var x -> S.singleton x
            Let v a b ->
                free (egr^._class a._data) <> S.delete v (free (egr^._class b._data))
            Lam v a -> S.delete v (free (egr^._class a._data))
            LFix v a -> S.delete v (free (egr^._class a._data))
            _ -> mconcat (map (\i -> free $ egr^._class i._data) (children n))

          cnst = evalL egr (unNode n)
       in
          Data freeVs cnst

    joinA (Data fv1 c1) (Data fv2 c2) =
        Data (fv1 `S.intersection` fv2) (c1 <|> c2)

    -- modifyA :: ClassId -> EGraph l -> EGraph l
    modifyA i egr = 
        case constant (egr^._class i._data) of
          Nothing -> egr
          Just c -> snd $ runEGraphM egr $ do
            new_c <- represent c
            GM.merge i new_c

instance Language Lambda

instance Num (Fix Lambda) where
    fromInteger = Fix . Num . fromInteger
    (+) = error "todo..."
    (-) = error "todo..."
    (*) = error "todo..."
    abs = error "todo..."
    signum = error "todo..."

rules :: [Rewrite Lambda]
rules =
    [ ifP trP "x" "y" := "x"
    , ifP flP "x" "y" := "y"
    -- , ifP (pat $ eq (varP "x") "e" "then" "else") := "else" :| if ...
    ]

rewrite :: Fix Lambda -> Fix Lambda
rewrite e = fst $ equalitySaturation e rules depthCost

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda"
    [ testCase "if tr" $
        rewrite (ifL tr 1 2) @?= 1

    , testCase "if fl" $
        rewrite (ifL fl 1 2) @?= 2
    ]




ifP :: Pattern Lambda -> Pattern Lambda -> Pattern Lambda -> Pattern Lambda
ifP a b c = pat (If a b c)
trP, flP :: Pattern Lambda
trP = pat (Bool True)
flP = pat (Bool False)
varP :: Pattern Lambda -> Pattern Lambda
varP x = pat (Var x)

-- TODO: recursion-schemes extension in separate package
ifL :: Fix Lambda -> Fix Lambda -> Fix Lambda -> Fix Lambda
ifL a b c = Fix (If a b c)
tr, fl :: Fix Lambda
tr = Fix $ Bool True
fl = Fix $ Bool False
