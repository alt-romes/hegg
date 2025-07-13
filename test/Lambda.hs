{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
module Lambda where

import Data.String

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe

import qualified Data.Set as S

import Control.Applicative ((<|>))

import Data.Equality.Graph
import Data.Equality.Extraction
import Data.Equality.Analysis
import Data.Equality.Saturation
import Data.Equality.Matching
import Data.Equality.Matching.Database as D
import Data.Equality.Graph.Lens

data Lambda a
    = Bool !Bool
    | Num !Int
    | Symbol !String
    | Use a
    | Subst a a a

    | Add a a
    | Eq a a
    | If a a a

    | App a a
    | Lam a a
    | Let a a a
    | LFix a a
    deriving ( Eq, Ord, Show
             , Functor, Foldable, Traversable
             )

evalL :: Lambda (Maybe (Lambda ())) -> Maybe (Lambda ())
evalL = \case
    Bool n -> Just (Bool n)
    Num n  -> Just (Num n)
    Add a b -> do
        a' <- a >>= num
        b' <- b >>= num
        return (Num $ a' + b')
    Eq  a b -> do
        a' <- a
        b' <- b
        return (Bool $ a' == b')
    _ -> Nothing
  where
    num :: Lambda () -> Maybe Int
    num = \case
        Num i -> Just i
        _ -> Nothing

type FreeVars = S.Set String
-- the lambda evaluator analysis is a combined analysis of the free variable analysis and the constant folding analysis
type LA = (FreeVars, Maybe (Lambda ()))

-- Constant folding for lambda evaluator
instance Analysis (Maybe (Lambda ())) Lambda where
  makeA = evalL
  joinA = (<|>)
  modifyA c eg
    = case eg^._class c._data of
        Nothing -> eg
        Just v  -> let (c', eg') = represent (f v) eg
                    in snd $ merge c c' eg'
          where
            f = \case
              Bool b -> Fix $ Bool b
              Num i  -> Fix $ Num i
              _ -> error "impossible, lambda () can't construct this"
                      

-- Free variable analysis for lambda
instance Analysis FreeVars Lambda where
  makeA = \case
    Use x -> x
    Let v a b -> (b S.\\ v) <> a
    Lam v a -> a S.\\ v
    LFix v a -> a S.\\ v
    Bool _ -> mempty
    Num _  -> mempty
    Add a b -> a <> b
    Eq a b -> a <> b
    App a b -> a <> b
    If a b c -> a <> b <> c
    Symbol x -> S.singleton x
    Subst a b c -> b <> a <> c

  joinA = (<>)

instance Num (Fix Lambda) where
    fromInteger = Fix . Num . fromInteger
    (+) a b = Fix $ Add a b
    (-) = error "todo..."
    (*) = error "todo..."
    abs = error "todo..."
    signum = error "todo..."

unsafeGetSubst :: Pattern Lambda -> VarsState -> D.Subst -> ClassId
unsafeGetSubst (NonVariablePattern _) _ _ = error "unsafeGetSubst: NonVariablePattern; expecting VariablePattern"
unsafeGetSubst (VariablePattern v) vss subst = findSubst (findVarName vss v) subst

isConst :: Pattern Lambda -> RewriteCondition LA Lambda
isConst v vss subst egr = isJust $ snd $ egr^._class (unsafeGetSubst v vss subst)._data

isNotSameVar :: Pattern Lambda -> Pattern Lambda -> RewriteCondition LA Lambda
isNotSameVar v1 v2 vss subst egr = find (unsafeGetSubst v1 vss subst) egr /= find (unsafeGetSubst v2 vss subst) egr

rules :: [Rewrite LA Lambda]
rules =
    [ ifP trP "x" "y" := "x"
    , ifP flP "x" "y" := "y"
    -- , ifP (pat $ Eq (pat $ Use "x") "e") "then" "else" := "else" :| conditionEqual (pat $ Let "x" "e" "then") (pat $ Let "x" "e" "else")

    , pat (Add "x" "y") := pat (Add "y" "x")
    , pat (Add (pat $ Add "x" "y") "z") := pat (Add "x" $ pat $ Add "y" "z")
    , pat (Eq "x" "y") := pat (Eq "y" "x")

    -- substitution introduction
    , pat (LFix "v" "e") := pat (Let "v" (pat $ LFix "v" "e") "e")
    , pat (App (pat $ Lam "v" "body") "e") := pat (Let "v" "e" "body")

    -- substitution propagation
    , pat (Let "v" "e" (pat $ App "a" "b")) := pat (App (pat $ Let "v" "e" "a") (pat $ Let "v" "e" "b"))
    , pat (Let "v" "e" (pat $ Add "a" "b")) := pat (Add (pat $ Let "v" "e" "a") (pat $ Let "v" "e" "b"))
    , pat (Let "v" "e" (pat $ Eq "a" "b")) := pat (Eq (pat $ Let "v" "e" "a") (pat $ Let "v" "e" "b"))
    , pat (Let "v" "e" (pat $ If "a" "b" "c")) := pat (If (pat $ Let "v" "e" "a") (pat $ Let "v" "e" "b") (pat $ Let "v" "e" "c"))

    -- substitution elimination
    , pat (Let "v" "e" "c") := "c" :| isConst "c" -- let const
    , pat (Let "v1" "e" (pat $ Use "v1")) := "e" -- let var same
    , pat (Let "v1" "e" (pat $ Use "v2")) := "v2" :| isNotSameVar "v1" "v2" -- let var diff
    , pat (Let "v1" "e" (pat $ Lam "v1" "body")) := pat (Lam "v1" "body") -- let lam same
    ]

rewrite :: Fix Lambda -> Fix Lambda
rewrite e = fst $ equalitySaturation e rules depthCost

lambdaTests :: TestTree
lambdaTests = testGroup "Lambda"
    [ testCase "if tr" $
        rewrite (ifL tr 1 2) @?= 1

    , testCase "if fl" $
        rewrite (ifL fl 1 2) @?= 2

    , testCase "lambda_under" $
      -- \x -> 4 + ((\y -> y) 4) = \x -> 8
        rewrite (lam "x" (4 + app (lam "y" (var "y")) 4)) @?= lam "x" 8

    {-
       This test requires at least the ConditionEqual rewrite condition helper
       and possibly dynamic rewrites. It would also be better to improve
       rewrite conditions before continuing down this path.

       For the analysis patch, being able to define the analysis
       compositionally and without expressiveness problems is good enough.

    , testCase "lambda_compose_many" $
        rewrite (Fix (Let "compose" (lam "f" (lam "g" (lam "x" (app (var "f") (app (var "g") (var "x"))))))
                          (Fix $ Let "add1" (lam "y" (Fix $ Add (var "y") 1)) (app (app (var "compose") (var "add1"))
                                                                                   (app (app (var "compose") (var "add1"))
                                                                                        (app (app (var "compose") (var "add1"))
                                                                                             (var "add1"))))))) @?= lam "x" (Fix $ Add "x" 5)
                                                                                             -}
    ]

ifP :: Pattern Lambda -> Pattern Lambda -> Pattern Lambda -> Pattern Lambda
ifP a b c = pat (If a b c)
trP, flP :: Pattern Lambda
trP = pat (Bool True)
flP = pat (Bool False)

-- TODO: recursion-schemes extension in separate package
ifL :: Fix Lambda -> Fix Lambda -> Fix Lambda -> Fix Lambda
ifL a b c = Fix (If a b c)
tr, fl :: Fix Lambda
tr = Fix $ Bool True
fl = Fix $ Bool False
lam :: Fix Lambda -> Fix Lambda -> Fix Lambda
lam i = Fix . Lam i
var :: Fix Lambda -> Fix Lambda
var = Fix . Use
app :: Fix Lambda -> Fix Lambda -> Fix Lambda
app x y = Fix $ App x y

instance IsString (Fix Lambda) where
  fromString = Fix . Symbol
