{-# OPTIONS_GHC -Wno-orphans #-} -- Arbitrary
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Invariants where

import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding (classes)

import Control.Monad

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as M

import Data.Equality.Graph
import Data.Equality.Saturation
import Data.Equality.Matching
import Database
import Sym


-- | When a rewrite of type "x":=c where x is a pattern variable and c is a
-- constant is used in equality saturation of any expression, all e-classes
-- should be merged into a single one, since all classes are equal to c and
-- therefore equivalent to themselves
patFoldAllClasses :: forall l. (Language l, Num (Pattern l))
                  => Fix l -> Integer -> Bool
patFoldAllClasses expr i =
    case IM.toList $ classes eg of
        [_] -> True
        _   -> False
    where
        eg :: EGraph l
        eg = snd $ equalitySaturation expr [VariablePattern "x":=fromInteger i] (error "Cost function shouldn't be used")

-- | Test 'compileToQuery'.
--
-- Every pattern compiled to a query should have the same number of free variables (except for the root variable)
-- as the pattern
--
-- The number of atoms should also match the number of non variable patterns
-- since we should create an additional atom (with a new bound variable) for each. 
testCompileToQuery :: Traversable lang => Pattern lang -> Bool
testCompileToQuery p = case compileToQuery p of
                         -- Handle special case for selectAll queries...
                         SelectAllQuery x -> [x] == vars p && numNonVarPatterns p == 0
                         q@(Query _ atoms)
                           | [] <- queryHeadVars q   -> False
                           | _:xs <- queryHeadVars q ->
                               L.sort xs == L.sort (vars p)
                                 && length atoms == numNonVarPatterns p
                         _ -> error "impossible! testCompileToQuery"
    where
        numNonVarPatterns :: Foldable lang => Pattern lang -> Int
        numNonVarPatterns (VariablePattern _) = 0
        numNonVarPatterns (NonVariablePattern l) = foldr ((+) . numNonVarPatterns) 1 l


-- | If we match a singleton variable pattern against an e-graph, we should get
-- a match on all e-classes in the e-graph
ematchSingletonVar :: Language lang => Var -> EGraph lang -> Bool
ematchSingletonVar v eg =
    let
        matches = S.fromList $ map snd $ ematch (VariablePattern v) eg
        eclasses = S.fromList $ map fst $ IM.toList $ classes eg
    in
        matches == eclasses 


-- | Property test for 'genericJoin'.
--
-- If we search a database with an expression in which all patterns are
-- variables (the only non-variable pattern is the top one), then, altogether,
-- we should get a list of all e-classes 
-- genericJoinAll :: Database lang -> 


-- The equivalence relation over e-nodes must be closed over congruence after rebuilding
-- congruenceInvariant :: Testable m (EGraph lang) => Property m


-- The hashcons 𝐻  must map all canonical e-nodes to their e-class ids
--
-- Note: the e-graph argument must have been rebuilt -- checking the property
-- when invariants are broken for sure doesn't make much sense
--
-- ROMES:TODO Should I rebuild it here? Then the property test is that after rebuilding ...HashConsInvariant
hashConsInvariant :: forall l. Language l
                  => EGraph l -> Bool
hashConsInvariant eg@(EGraph {..}) =
    all f (IM.toList classes)
    where
      -- e-node 𝑛 ∈ 𝑀 [𝑎] ⇐⇒ 𝐻 [canonicalize(𝑛)] = find(𝑎)
      f (i, EClass _ nodes _) = all g nodes
        where
          g en = case M.lookup (canonicalize en eg) memo of
            Nothing -> error "how can we not find canonical thing in map? :)" -- False
            Just i' -> i' == find i eg 

-- ROMES:TODO: Property: Extract expression after equality saturation is always better or equal to the original expression

-- ROMES:TODO: Use action trick https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
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

instance Arbitrary BOp where
    arbitrary = oneof [ return Add
                      , return Sub
                      , return Mul
                      , return Div ]

instance Arbitrary UOp where
    arbitrary = oneof [ return Negate
                      -- , return Sin
                      -- , return Cos
                      ]

instance Arbitrary a => Arbitrary (Expr a) where
    arbitrary = sized expr'
        where
            expr' :: Int -> Gen (Expr a)
            expr' 0 = oneof [ Sym . un <$> arbitrary
                            , Const . fromInteger <$> arbitrary
                            ]
            expr' n
              | n > 0 = oneof [ BinOp <$> arbitrary <*> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                              , UnOp <$> arbitrary <*> resize (n - 1) arbitrary ]
            expr' _ = error "size is negative?"

instance Arbitrary (Fix Expr) where
    arbitrary = Fix <$> arbitrary

instance Arbitrary (Pattern Expr) where
    arbitrary = sized p'
      where
        p' 0 = VariablePattern . un <$> arbitrary
        p' n = NonVariablePattern <$> resize (n `div` 2) arbitrary

newtype Name = Name { un :: String }

instance Arbitrary Name where
  arbitrary = oneof (return . Name . (:[]) <$> ['a'..'l'])

invariants :: TestTree
invariants = testGroup "Invariants"
  [ QC.testProperty "Compile to query" (testCompileToQuery @Expr)
  , QC.testProperty "Singleton variable matches all" (ematchSingletonVar @Expr)
  , QC.testProperty "Hash Cons Invariant" (hashConsInvariant @Expr)
  , QC.testProperty "Fold all classes with x:=c" (patFoldAllClasses @Expr)
  ]

