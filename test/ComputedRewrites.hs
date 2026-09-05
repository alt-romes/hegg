{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module ComputedRewrites (computedRewriteTests) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (forM_, guard, void)
import Data.List (isInfixOf)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Data.Equality.Analysis
import qualified Data.Equality.Graph as G
import Data.Equality.Graph.Internal (classes, memo)
import Data.Equality.Graph.Lens
import qualified Data.Equality.Graph.Monad as EG
import Data.Equality.Matching
import Data.Equality.Matching.Database (Subst, findSubst)
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

data Expr a = Number Integer | Symbol String | Add a a | Neg a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- No modifyA: knowing a constant must not materialize a literal by itself.
instance Analysis (Maybe Integer) Expr where
    makeA = constantValue
    joinA = joinConstant

constantValue :: Expr (Maybe Integer) -> Maybe Integer
constantValue = \case
    Number n -> Just n
    Symbol _ -> Nothing
    Add a b -> (+) <$> a <*> b
    Neg a -> negate <$> a

joinConstant :: Maybe Integer -> Maybe Integer -> Maybe Integer
joinConstant Nothing b = b
joinConstant a Nothing = a
joinConstant a b
    | a == b = a
    | otherwise = error "computed rewrite equated distinct constants"

cost :: CostFunction analysis Expr Int
cost = costOnly $ \node -> 1 + sum node

number :: Integer -> Fix Expr
number = Fix . Number

valueOf :: String -> VarsState -> Subst -> G.EGraph (Maybe Integer) Expr -> Maybe Integer
valueOf name vars subst graph =
    graph ^. _class (findSubst (findVarName vars name) subst) . _data

foldAddition :: Rewrite (Maybe Integer) Expr
foldAddition = ComputePattern (pat $ Add "a" "b") $ \vars subst graph -> do
    a <- valueOf "a" vars subst graph
    b <- valueOf "b" vars subst graph
    pure $ pat $ Number $ a + b

combineConstants :: Rewrite (Maybe Integer) Expr
combineConstants = ComputePattern (pat $ Add (pat $ Add "x" "a") "b") $ \vars subst graph -> do
    a <- valueOf "a" vars subst graph
    b <- valueOf "b" vars subst graph
    pure $ pat $ Add "x" (pat $ Number $ a + b)

foldNegation :: PatternRewriteFun (Maybe Integer) Expr
foldNegation vars subst graph =
    pat . Number . negate <$> valueOf "x" vars subst graph

defineX :: Rewrite analysis Expr
defineX = pat (Symbol "x") := pat (Number 2)

unknownSum :: Fix Expr
unknownSum = Fix $ Add (Fix $ Symbol "x") (number 3)

computedRewriteTests :: TestTree
computedRewriteTests = testGroup "Computed rewrites"
    [ testCase "materializes a literal from analysis without modifyA" $ do
        let input = Fix $ Add (number 2) (number 3)
            unchanged = equalitySaturation input ([] :: [Rewrite (Maybe Integer) Expr]) cost
            rewritten = equalitySaturation input [foldAddition] cost
        fst unchanged @?= input
        fst rewritten @?= number 5
        G.lookupNM (G.Node $ Number 5) (memo $ snd unchanged) @?= Nothing

    , testCase "combines constants while preserving an unknown compound capture" $ do
        let captured = Fix $ Neg $ Fix $ Symbol "x"
            input = Fix $ Add (Fix $ Add captured (number 2)) (number 3)
            expected = Fix $ Add captured (number 5)
            (best, graph) = equalitySaturation input [combineConstants] cost
            matchedClasses expression =
                map (\match -> G.find (matchClassId match) graph) $
                    ematch (eGraphToDatabase graph) (fst $ compileToQuery $ cata pat expression)
            originalClasses = matchedClasses input
        best @?= expected
        assertBool "the source expression is retained" $ not $ null originalClasses
        assertBool "the replacement is equivalent to the source" $
            any (`elem` originalClasses) (matchedClasses expected)

    , testCase "refuses to combine when either constant is unknown" $ do
        let captured = Fix $ Neg $ Fix $ Symbol "x"
            unknown = Fix $ Symbol "unknown"
        forM_ [(unknown, number 3), (number 2, unknown)] $ \(a, b) -> do
            let input = Fix $ Add (Fix $ Add captured a) b
                (_, before) = EG.egraph $ EG.represent input <* EG.rebuild
                matches = ematch (eGraphToDatabase before) $
                    fst $ compileToQuery $ rewriteLhs combineConstants
                (best, after) = equalitySaturation input [combineConstants] cost
            assertBool "the LHS matches before the builder refuses" $ not $ null matches
            best @?= input
            G.unNodeMap (memo after) @?= G.unNodeMap (memo before)
            classContents after @?= classContents before

    , testCase "reuses nested captures after an earlier rule merges their classes" $ do
        let ((root, oldX, two), graph) = EG.egraph $ do
                existingTwo <- EG.represent $ number 2
                -- Give 2 more parents than x so x's captured ID is subsumed.
                void $ EG.represent $ Fix $ Neg $ number 2
                void $ EG.represent $ Fix $ Add (number 2) (number 7)
                capturedX <- EG.represent $ Fix $ Symbol "x"
                original <- EG.represent $ Fix $ Neg unknownSum
                runEqualitySaturation defaultBackoffScheduler [defineX, distributeNegation]
                pure (original, capturedX, existingTwo)
            expected = Fix $ Add (Fix $ Neg $ number 2) (number (-3))
            (expectedClass, withExpected) = G.represent expected graph
        assertBool "the original capture ID was merged away" $ G.find oldX graph /= oldX
        G.find oldX graph @?= G.find two graph
        G.find expectedClass withExpected @?= G.find root withExpected

    , testCase "a computed variable RHS merges with the captured class" $ do
        let rule = ComputePattern (pat $ Neg $ pat $ Neg "x") $ \_ _ _ -> Just "x"
            input = Fix $ Neg $ Fix $ Neg $ Fix $ Symbol "x"
        fst (equalitySaturation input [rule :: Rewrite (Maybe Integer) Expr] cost)
            @?= Fix (Symbol "x")

    , testCase "refusals and nested guards leave the graph unchanged" $ do
        let (_, graph) = EG.egraph $ EG.represent unknownSum <* EG.rebuild
            lhs = pat $ Add "a" "b"
            rules =
                [ foldAddition
                , ComputePattern lhs (\_ _ _ -> Nothing)
                , ComputePattern lhs (\_ _ _ -> error "a rejected builder was evaluated")
                    :| (\_ _ _ -> False) :| (\_ _ _ -> True)
                ]
        forM_ rules $ \rule -> do
            let (_, after) = EG.runEGraphM graph $
                    runEqualitySaturation defaultBackoffScheduler [rule]
            G.unNodeMap (memo after) @?= G.unNodeMap (memo graph)
            classContents after @?= classContents graph

    , testCase "reports an unbound variable in a computed RHS" $ do
        let rule = ComputePattern (pat $ Number 1) $ \_ _ _ ->
                Just $ pat $ Add (pat $ Number 99) "unbound"
            result = equalitySaturation (number 1) [rule :: Rewrite (Maybe Integer) Expr] cost
        failure <- try (evaluate $ fst result) :: IO (Either ErrorCall (Fix Expr))
        case failure of
            Left err -> assertBool (show err) $
                "unbound RHS variable \"unbound\"" `isInfixOf` show err
            Right expression -> assertFailure $ "accepted malformed RHS: " <> show expression

    , testCase "retries a refused builder after an ordinary rewrite supplies facts" $
        fst (equalitySaturation (Fix $ Neg unknownSum)
            [ComputePattern (pat $ Neg "x") foldNegation, defineX] cost) @?= number (-5)

    , testCase "retries computed rules after scheduler backoff" $
        fst (equalitySaturation' (BackoffScheduler 0 10) (Fix $ Neg unknownSum)
            [ComputePattern (pat $ Neg "x") foldNegation, defineX] cost) @?= number (-5)

    , testCase "retries after analysis changes with unchanged node and class counts" $ do
        let input = Fix $ Neg unknownSum
            (root, before) = EG.egraph $ EG.represent input <* EG.rebuild
            (_, produced) = EG.runEGraphM before $
                runEqualitySaturation defaultBackoffScheduler [defineX :: Rewrite PrunedConstant Expr]
            negateKnown = ComputePattern (pat $ Neg "x") $ \vars subst graph -> do
                let PrunedConstant value = graph ^.
                        _class (findSubst (findVarName vars "x") subst) . _data
                pat . Number . negate <$> value
            result = equalitySaturation input [negateKnown, defineX :: Rewrite PrunedConstant Expr] cost
        G.sizeNM (memo produced) @?= G.sizeNM (memo before)
        IM.size (classes produced) @?= IM.size (classes before)
        produced ^. _class root . _data @?= PrunedConstant (Just (-5))
        G.lookupNM (G.Node $ Number (-5)) (memo produced) @?= Nothing
        fst result @?= number (-5)
    ]
  where
    distributeNegation = ComputePattern (pat $ Neg $ pat $ Add "x" "offset") $ \vars subst graph -> do
        x <- valueOf "x" vars subst graph
        guard $ x == 2
        offset <- valueOf "offset" vars subst graph
        pure $ pat $ Add (pat $ Neg "x") (pat $ Number $ negate offset)

    classContents = IM.map (\cl -> (G.eClassNodes cl, G.eClassData cl)) . classes

-- Pruning a redundant expression after its equivalent literal appears is an
-- idempotent analysis hook. Replacing Symbol x with Number 2 keeps both counts
-- unchanged while learning a value; parent facts become available at rebuild.
newtype PrunedConstant = PrunedConstant (Maybe Integer)
    deriving (Eq, Show)

instance Analysis PrunedConstant Expr where
    makeA = PrunedConstant . constantValue . fmap (\(PrunedConstant value) -> value)
    joinA (PrunedConstant a) (PrunedConstant b) = PrunedConstant $ joinConstant a b
    modifyA classId graph = case graph ^. _class classId . _data of
        PrunedConstant (Just n)
            | let literal = G.Node $ Number n
            , let nodes = graph ^. _class classId . _nodes
            , S.member literal nodes ->
                let removed = S.delete literal nodes
                    pruned = over (_class classId . _nodes) (const $ S.singleton literal) graph
                in over _memo (\nodeMap -> S.foldr G.deleteNM nodeMap removed) pruned
        _ -> graph
