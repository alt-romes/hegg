{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
-- | Tests and benchmarks for associative-commutative (A/C) canonicalization
-- via 'normalizeNode', following the approach from "Custom Data Structures
-- in E-Graphs" (Shanabrook, UW PLSE, 2026).
--
-- Two languages are defined:
--
-- * 'BinExpr' — binary operators, A/C handled via rewrite rules (blows up)
-- * 'MSExpr' — n-ary multiset operators, A/C handled structurally (no blowup)
module AC
    ( acTests
    , reportBlowup
    , BlowupMetrics(..)
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List (sort)
import Data.String (IsString(..))
import System.CPUTime (getCPUTime)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import Data.Equality.Graph
import Data.Equality.Graph.Internal (EGraph(..))
import Data.Equality.Graph.Monad (runEGraphM)
import Data.Equality.Matching
import Data.Equality.Extraction
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler

data BinExpr a
    = BVar  !String
    | BConst !Int
    | BAdd  !a !a
    | BMul  !a !a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Language BinExpr  -- default normalizeNode = id

-- Analysis () BinExpr is provided by the default instance: Analysis () l

instance IsString (Fix BinExpr) where
    fromString = Fix . BVar

instance Num (Fix BinExpr) where
    (+) a b = Fix (BAdd a b)
    (*) a b = Fix (BMul a b)
    fromInteger = Fix . BConst . fromInteger
    negate = error "BinExpr: negate"
    abs = error "BinExpr: abs"
    signum = error "BinExpr: signum"

binCost :: CostFunction () BinExpr Int
binCost = costOnly $ \case
    BVar _    -> 1
    BConst _  -> 1
    BAdd c1 c2 -> c1 + c2 + 1
    BMul c1 c2 -> c1 + c2 + 1

-- A/C rewrite rules for binary language
binACRules :: [Rewrite () BinExpr]
binACRules =
    [ -- commutativity of addition
      pat (BAdd "a" "b") := pat (BAdd "b" "a")
      -- associativity of addition
    , pat (BAdd "a" (pat (BAdd "b" "c"))) := pat (BAdd (pat (BAdd "a" "b")) "c")
    ]

data MSExpr a
    = MSVar   !String
    | MSConst !Int
    | MSAdd   ![a]          -- n-ary A/C addition
    | MSMul   ![a]          -- n-ary A/C multiplication
    | MSSub   !a !a         -- binary subtraction (non-commutative)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Language MSExpr where
    normalizeNode (MSAdd xs) = MSAdd (sort xs)
    normalizeNode (MSMul xs) = MSMul (sort xs)
    normalizeNode other      = other

acTests :: TestTree
acTests = testGroup "A/C Canonicalization"
    [ testGroup "Basic"
        [ testCommutativity
        , testMultiplePermutations
        , testNonCommutativeSub
        , testCongruence
        ]
    , testGroup "Equivalence with binary+rewrites"
        [ testEquivPairwise
        , testEquivTriplewise
        , testEquivNested
        , testEquivMulCommutativity
        , testEquivMixed
        , testEquivWithConstants
        , testEquivRepeatedVars
        ]
    , testBlowupReport
    ]

-- | MSAdd [a, b] and MSAdd [b, a] should be the same e-class
testCommutativity :: TestTree
testCommutativity = testCase "commutativity: MSAdd [a,b] == MSAdd [b,a]" $ do
    let a = Fix (MSVar "a")
        b = Fix (MSVar "b")
        ab = Fix (MSAdd [a, b])
        ba' = Fix (MSAdd [b, a])
        (c1, eg1) = represent ab (emptyEGraph :: EGraph () MSExpr)
        (c2, eg2) = represent ba' eg1
    find c1 eg2 @?= find c2 eg2

-- | All permutations of MSAdd [a, b, c] should be the same e-class
testMultiplePermutations :: TestTree
testMultiplePermutations = testCase "all permutations of MSAdd [a,b,c] same class" $ do
    let a = Fix (MSVar "a")
        b = Fix (MSVar "b")
        c = Fix (MSVar "c")
        perms = [ [a,b,c], [a,c,b], [b,a,c], [b,c,a], [c,a,b], [c,b,a] ]
        exprs = map (Fix . MSAdd) perms
        -- Represent all permutations
        (ids, finalEg) = foldr
            (\expr (acc, eg) -> let (cid, eg') = represent expr eg in (cid:acc, eg'))
            ([], emptyEGraph :: EGraph () MSExpr)
            exprs
        -- All should be in the same canonical class
        canonIds = map (`find` finalEg) ids
    assertBool "all permutations should have the same canonical id"
        (case canonIds of
            []       -> True
            cId:rest  -> all (== cId) rest)

-- | MSSub a b /= MSSub b a (non-commutative)
testNonCommutativeSub :: TestTree
testNonCommutativeSub = testCase "non-commutative: MSSub a b /= MSSub b a" $ do
    let a = Fix (MSVar "a")
        b = Fix (MSVar "b")
        ab = Fix (MSSub a b)
        ba' = Fix (MSSub b a)
        (c1, eg1) = represent ab (emptyEGraph :: EGraph () MSExpr)
        (c2, eg2) = represent ba' eg1
    assertBool "MSSub a b and MSSub b a should be different classes"
        (find c1 eg2 /= find c2 eg2)

-- | Congruence: after merging a=b, MSAdd [a,c] should equal MSAdd [b,c]
-- This tests that normalizeNode is called during rebuild/repair
testCongruence :: TestTree
testCongruence = testCase "congruence: merge a=b updates MSAdd [a,c]" $ do
    let a = Fix (MSVar "a")
        b = Fix (MSVar "b")
        c = Fix (MSVar "c")
        ac = Fix (MSAdd [a, c])
        bc' = Fix (MSAdd [b, c])
        (idA, eg1) = represent a (emptyEGraph :: EGraph () MSExpr)
        (idB, eg2) = represent b eg1
        (idAC, eg3) = represent ac eg2
        (idBC, eg4) = represent bc' eg3
        -- Before merge: MSAdd [a,c] /= MSAdd [b,c]
    assertBool "before merge: MSAdd [a,c] /= MSAdd [b,c]"
        (find idAC eg4 /= find idBC eg4)

    let -- Merge a = b
        (_, eg5) = merge idA idB eg4
        eg6 = rebuild eg5
    -- After merge: MSAdd [a,c] should equal MSAdd [b,c] via congruence
    assertBool "after merge a=b: MSAdd [a,c] == MSAdd [b,c]"
        (find idAC eg6 == find idBC eg6)

-- | Helper: check whether two binary expressions are equivalent after
-- A/C saturation. Uses NoBanScheduler for full saturation.
binEquiv :: Fix BinExpr -> Fix BinExpr -> Bool
binEquiv e1 e2 =
    let (id1, eg1) = represent e1 (emptyEGraph :: EGraph () BinExpr)
        (id2, eg2) = represent e2 eg1
        -- Run A/C rewrites to saturation on the graph containing both expressions
        (_, eg3) = runEGraphM eg2 (runEqualitySaturation NoBanScheduler binACRules)
    in find id1 eg3 == find id2 eg3

-- | Helper: check whether two multiset expressions are equivalent
-- immediately after insertion (no rewrites needed).
msEquiv :: Fix MSExpr -> Fix MSExpr -> Bool
msEquiv e1 e2 =
    let (id1, eg1) = represent e1 (emptyEGraph :: EGraph () MSExpr)
        (id2, eg2) = represent e2 eg1
    in find id1 eg2 == find id2 eg2

-- | Helpers for constructing binary expressions
bv :: String -> Fix BinExpr
bv = Fix . BVar
ba :: Fix BinExpr -> Fix BinExpr -> Fix BinExpr
ba x y = Fix (BAdd x y)

-- | Helpers for constructing multiset expressions
mv :: String -> Fix MSExpr
mv = Fix . MSVar
mc :: Int -> Fix MSExpr
mc = Fix . MSConst
ms :: [Fix MSExpr] -> Fix MSExpr
ms = Fix . MSAdd
mp :: [Fix MSExpr] -> Fix MSExpr
mp = Fix . MSMul

-- | a+b == b+a: both approaches should agree
testEquivPairwise :: TestTree
testEquivPairwise = testCase "pairwise commutativity: a+b == b+a" $ do
    -- Binary: a+b should be equiv to b+a after A/C rewrites
    assertBool "binary: a+b == b+a" (binEquiv (ba (bv "a") (bv "b")) (ba (bv "b") (bv "a")))
    -- Multiset: immediate
    assertBool "multiset: a+b == b+a" (msEquiv (ms [mv "a", mv "b"]) (ms [mv "b", mv "a"]))

-- | All bracketings of a+b+c: (a+b)+c == a+(b+c) == (a+c)+b == ...
testEquivTriplewise :: TestTree
testEquivTriplewise = testCase "all bracketings of a+b+c are equivalent" $ do
    let a = bv "a"; b = bv "b"; c = bv "c"
    -- Binary: (a+b)+c should be equiv to a+(b+c) after A/C rewrites
    assertBool "binary: (a+b)+c == a+(b+c)"
        (binEquiv (ba (ba a b) c) (ba a (ba b c)))
    assertBool "binary: (a+b)+c == (c+a)+b"
        (binEquiv (ba (ba a b) c) (ba (ba c a) b))
    -- Multiset: all orderings of [a,b,c] are immediately equivalent
    let ma = mv "a"; mb = mv "b"; mc' = mv "c"
    assertBool "multiset: [a,b,c] == [c,b,a]"
        (msEquiv (ms [ma, mb, mc']) (ms [mc', mb, ma]))
    assertBool "multiset: [a,b,c] == [b,a,c]"
        (msEquiv (ms [ma, mb, mc']) (ms [mb, ma, mc']))

-- | Nested sums: (a+b)+(c+d) should have all 24 rearrangements equivalent
testEquivNested :: TestTree
testEquivNested = testCase "nested: (a+b)+(c+d) commutativity" $ do
    let a = bv "a"; b = bv "b"; c = bv "c"; d = bv "d"
    -- Binary: (a+b)+(c+d) == (c+d)+(a+b)
    assertBool "binary: (a+b)+(c+d) == (c+d)+(a+b)"
        (binEquiv (ba (ba a b) (ba c d)) (ba (ba c d) (ba a b)))
    -- Binary: (a+b)+(c+d) == (d+c)+(b+a)
    assertBool "binary: (a+b)+(c+d) == (d+c)+(b+a)"
        (binEquiv (ba (ba a b) (ba c d)) (ba (ba d c) (ba b a)))
    -- Multiset: flattened [a,b,c,d] == [d,c,b,a]
    let ma = mv "a"; mb = mv "b"; mc' = mv "c"; md = mv "d"
    assertBool "multiset: [a,b,c,d] == [d,c,b,a]"
        (msEquiv (ms [ma, mb, mc', md]) (ms [md, mc', mb, ma]))
    assertBool "multiset: [a,b,c,d] == [c,a,d,b]"
        (msEquiv (ms [ma, mb, mc', md]) (ms [mc', ma, md, mb]))

-- | Multiplication commutativity: a*b == b*a
testEquivMulCommutativity :: TestTree
testEquivMulCommutativity = testCase "multiplication commutativity" $ do
    let ma = mv "a"; mb = mv "b"; mc' = mv "c"
    -- MSMul uses normalizeNode too
    assertBool "multiset: a*b == b*a"
        (msEquiv (mp [ma, mb]) (mp [mb, ma]))
    assertBool "multiset: a*b*c == c*a*b"
        (msEquiv (mp [ma, mb, mc']) (mp [mc', ma, mb]))

-- | Mixed Add and Mul: (a*b) + (c*d) == (c*d) + (b*a)
-- The outer Add and inner Mul should both be commutative
testEquivMixed :: TestTree
testEquivMixed = testCase "mixed: Add over Mul commutativity" $ do
    let ma = mv "a"; mb = mv "b"; mc' = mv "c"; md = mv "d"
    -- (a*b) + (c*d) == (d*c) + (b*a)
    assertBool "multiset: (a*b)+(c*d) == (d*c)+(b*a)"
        (msEquiv (ms [mp [ma, mb], mp [mc', md]])
                 (ms [mp [md, mc'], mp [mb, ma]]))
    -- (a*b) + (c*d) /= (a*c) + (b*d)  (different terms!)
    assertBool "multiset: (a*b)+(c*d) /= (a*c)+(b*d)"
        (not $ msEquiv (ms [mp [ma, mb], mp [mc', md]])
                       (ms [mp [ma, mc'], mp [mb, md]]))

-- | Constants: 2+3+a == a+3+2 == 3+a+2
testEquivWithConstants :: TestTree
testEquivWithConstants = testCase "constants: 2+3+a in any order" $ do
    let two = mc 2; three = mc 3; a = mv "a"
    assertBool "multiset: [2,3,a] == [a,3,2]"
        (msEquiv (ms [two, three, a]) (ms [a, three, two]))
    assertBool "multiset: [2,3,a] == [3,a,2]"
        (msEquiv (ms [two, three, a]) (ms [three, a, two]))

-- | Repeated variables: a+a+b == b+a+a == a+b+a
testEquivRepeatedVars :: TestTree
testEquivRepeatedVars = testCase "repeated variables: a+a+b in any order" $ do
    let a = mv "a"; b = mv "b"
    assertBool "multiset: [a,a,b] == [b,a,a]"
        (msEquiv (ms [a, a, b]) (ms [b, a, a]))
    assertBool "multiset: [a,a,b] == [a,b,a]"
        (msEquiv (ms [a, a, b]) (ms [a, b, a]))
    -- But a+a+b /= a+b+b (different multiplicities)
    assertBool "multiset: [a,a,b] /= [a,b,b]"
        (not $ msEquiv (ms [a, a, b]) (ms [a, b, b]))

-- ============================================================
-- Blowup measurement
-- ============================================================

data BlowupMetrics = BlowupMetrics
    { bmTermCount :: !Int     -- n (number of terms in the sum)
    , bmEClasses  :: !Int     -- e-classes after saturation
    , bmENodes    :: !Int     -- e-nodes after saturation
    , bmTimeMs    :: !Double  -- wall-clock milliseconds
    } deriving (Show)

-- | Count e-classes in an e-graph
countEClasses :: EGraph a l -> Int
countEClasses = IM.size . classes

-- | Count e-nodes in an e-graph
countENodes :: Language l => EGraph a l -> Int
countENodes eg = sum [ S.size (eClassNodes c) | c <- IM.elems (classes eg) ]

-- | A scheduler that never bans any rules — runs to true saturation.
-- This ensures the binary approach fully explores all A/C equivalences,
-- showing the true blowup rather than hiding it behind rule banning.
data NoBanScheduler = NoBanScheduler

instance Scheduler l NoBanScheduler where
    data Stat l NoBanScheduler = NoBanStat
    updateStats :: NoBanScheduler
                -> Int
                -> Int
                -> Rewrite a l
                -> Maybe (Stat l NoBanScheduler)
                -> IM.IntMap (Stat l NoBanScheduler)
                -> [Match]
                -> IM.IntMap (Stat l NoBanScheduler)
    updateStats _ _ _ _ _ stats _ = stats
    isBanned :: Int -> Stat l NoBanScheduler -> Bool
    isBanned _ _ = False

-- | Build binary sum: c1 + (v1 + (v2 + (... + c2)))
buildBinarySum :: Int -> Fix BinExpr
buildBinarySum n =
    let terms = [Fix (BConst 2)]
             ++ [Fix (BVar ("v" ++ show i)) | i <- [1..n-2]]
             ++ [Fix (BConst 3)]
    in foldr1 (\a b -> Fix (BAdd a b)) terms

-- | Build multiset sum: MSAdd [c1, v1, v2, ..., c2]
buildMultisetSum :: Int -> Fix MSExpr
buildMultisetSum n =
    let terms = [Fix (MSConst 2)]
             ++ [Fix (MSVar ("v" ++ show i)) | i <- [1..n-2]]
             ++ [Fix (MSConst 3)]
    in Fix (MSAdd terms)

-- | Measure binary approach: add expression, run A/C rewrites to full
-- saturation (no rule banning), measure size.
measureBinary :: Int -> IO BlowupMetrics
measureBinary n = do
    let expr = buildBinarySum n
    t0 <- getCPUTime
    let (_, eg) = equalitySaturation' NoBanScheduler expr binACRules binCost
        nc = countEClasses eg
        nn = countENodes eg
    -- Force evaluation
    nc `seq` nn `seq` return ()
    t1 <- getCPUTime
    let timeMs = fromIntegral (t1 - t0) / 1e9
    return $ BlowupMetrics n nc nn timeMs

-- | Measure multiset approach: just represent the expression.
-- No A/C rewrites needed — normalizeNode handles everything at insertion time.
measureMultiset :: Int -> IO BlowupMetrics
measureMultiset n = do
    let expr = buildMultisetSum n
    t0 <- getCPUTime
    let (_, eg) = represent expr (emptyEGraph :: EGraph () MSExpr)
        nc = countEClasses eg
        nn = countENodes eg
    nc `seq` nn `seq` return ()
    t1 <- getCPUTime
    let timeMs = fromIntegral (t1 - t0) / 1e9
    return $ BlowupMetrics n nc nn timeMs

-- | Print a comparison table showing the A/C blowup.
-- Binary approach runs to full saturation (NoBanScheduler) to show true blowup.
-- Multiset approach just inserts the expression — no rewrites needed.
reportBlowup :: IO ()
reportBlowup = do
    putStrLn ""
    putStrLn "A/C Blowup: Binary+Rewrites (full saturation) vs Multiset+normalizeNode"
    putStrLn "========================================================================"
    putStrLn " n | Bin classes | Bin nodes |  Bin time | MS classes | MS nodes | MS time  | Node ratio"
    putStrLn "---+------------+-----------+----------+------------+----------+----------+-----------"
    -- Binary blows up fast — n=10 is already very large.
    -- Multiset stays linear, so we test it up to n=50.
    mapM_ reportRow [3, 5, 7, 9, 10]
    putStrLn "---+------------+-----------+----------+------------+----------+----------+-----------"
    putStrLn "Multiset-only (binary too large):"
    mapM_ reportRowMSOnly [15, 20, 30, 50, 100]
  where
    reportRow n = do
        bin <- measureBinary n
        msm <- measureMultiset n
        let ratio :: Double
            ratio = fromIntegral (bmENodes bin) / fromIntegral (max 1 (bmENodes msm))
        putStrLn $ padL 2 (show n)
            ++ " | " ++ padL 10 (show (bmEClasses bin))
            ++ " | " ++ padL 9 (show (bmENodes bin))
            ++ " | " ++ padL 8 (showTime (bmTimeMs bin))
            ++ " | " ++ padL 10 (show (bmEClasses msm))
            ++ " | " ++ padL 8 (show (bmENodes msm))
            ++ " | " ++ padL 8 (showTime (bmTimeMs msm))
            ++ " | " ++ padL 9 (showRatio ratio)

    reportRowMSOnly n = do
        msm <- measureMultiset n
        putStrLn $ padL 2 (show n)
            ++ " |          - |         - |        - "
            ++ "| " ++ padL 10 (show (bmEClasses msm))
            ++ " | " ++ padL 8 (show (bmENodes msm))
            ++ " | " ++ padL 8 (showTime (bmTimeMs msm))
            ++ " |         -"

    padL w s = replicate (w - length s) ' ' ++ s
    showRatio r
        | r >= 1000 = show (round r `div` 1000 :: Int) ++ "kx"
        | otherwise = show (round r :: Int) ++ "x"
    showTime ms'
        | ms' < 1    = "<1ms"
        | ms' < 1000 = show (round ms' :: Int) ++ "ms"
        | otherwise  = show (round (ms' / 1000) :: Int) ++ "s"

-- | Test that runs the blowup report and asserts multiset is always smaller
testBlowupReport :: TestTree
testBlowupReport = testCase "multiset approach produces fewer e-nodes than binary+rewrites" $ do
    reportBlowup
    -- For n=3..10, multiset should have strictly fewer nodes
    mapM_ (\n -> do
        bin <- measureBinary n
        ms' <- measureMultiset n
        assertBool ("n=" ++ show n ++ ": multiset (" ++ show (bmENodes ms')
                    ++ " nodes) should have fewer nodes than binary ("
                    ++ show (bmENodes bin) ++ " nodes)")
            (bmENodes ms' < bmENodes bin)
        ) [3, 5, 7, 9]
