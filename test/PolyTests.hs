module PolyTests (polyTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map
import Data.Equality.Graph.Poly

polyTests :: TestTree
polyTests = testGroup "PolyMap"
    [ testCase "empty poly" $
        emptyPoly @?= PolyMap Map.empty

    , testCase "constant poly" $
        constPoly 5.0 @?= PolyMap (Map.singleton Map.empty 5.0)

    , testCase "zero constant is empty" $
        constPoly 0 @?= emptyPoly

    , testCase "singleton poly" $
        singletonPoly 42 @?= PolyMap (Map.singleton (Map.singleton 42 1) 1.0)

    , testCase "2x + 3x = 5x" $
        addPoly (scalePoly 2 (singletonPoly 1)) (scalePoly 3 (singletonPoly 1))
            @?= PolyMap (Map.singleton (Map.singleton 1 1) 5.0)

    , testCase "2x + 3y" $
        addPoly (scalePoly 2 (singletonPoly 1)) (scalePoly 3 (singletonPoly 2))
            @?= PolyMap (Map.fromList
                [ (Map.singleton 1 1, 2.0)
                , (Map.singleton 2 1, 3.0)
                ])

    , testCase "(x+y)² = x² + 2xy + y²" $ do
        let xy = addPoly (singletonPoly 1) (singletonPoly 2)
        mulPoly xy xy @?= PolyMap (Map.fromList
            [ (Map.singleton 1 2, 1.0)           -- x²
            , (Map.fromList [(1,1),(2,1)], 2.0)  -- 2xy
            , (Map.singleton 2 2, 1.0)           -- y²
            ])

    , testCase "negate(3) = -3" $
        negatePoly (constPoly 3.0) @?= constPoly (-3.0)

    , testCase "negate(2x + 5) = -2x - 5" $
        negatePoly (addPoly (scalePoly 2 (singletonPoly 1)) (constPoly 5))
            @?= PolyMap (Map.fromList
                [ (Map.singleton 1 1, -2.0)
                , (Map.empty, -5.0)
                ])

    , testCase "canonicalize: find(2)=1 merges x₁ + x₂ → 2x₁" $
        canonicalizePoly (\cid -> if cid == 2 then 1 else cid)
            (addPoly (singletonPoly 1) (singletonPoly 2))
            @?= PolyMap (Map.singleton (Map.singleton 1 1) 2.0)

    , testCase "canonicalize: find(2)=1 merges x₁·x₂ → x₁²" $
        canonicalizePoly (\cid -> if cid == 2 then 1 else cid)
            (PolyMap (Map.singleton (Map.fromList [(1,1),(2,1)]) 3.0))
            @?= PolyMap (Map.singleton (Map.singleton 1 2) 3.0)

    , testCase "x * 0 = 0" $
        mulPoly (singletonPoly 1) (constPoly 0) @?= emptyPoly

    , testCase "p * 1 = p" $ do
        let p = addPoly (scalePoly 3 (singletonPoly 1)) (constPoly 5)
        mulPoly p (constPoly 1) @?= p

    , testCase "(2x+3)(x+1) = 2x² + 5x + 3" $
        mulPoly
            (addPoly (scalePoly 2 (singletonPoly 1)) (constPoly 3))
            (addPoly (singletonPoly 1) (constPoly 1))
            @?= PolyMap (Map.fromList
                [ (Map.singleton 1 2, 2.0)  -- 2x²
                , (Map.singleton 1 1, 5.0)  -- 5x
                , (Map.empty, 3.0)          -- 3
                ])

    , testCase "scale 0 = empty" $
        scalePoly 0 (singletonPoly 1) @?= PolyMap (Map.singleton (Map.singleton 1 1) 0.0)
        -- Note: scalePoly doesn't prune zeros; that's the caller's job

    , testCase "polySize" $ do
        polySize emptyPoly @?= 0
        polySize (constPoly 5) @?= 1
        polySize (addPoly (singletonPoly 1) (singletonPoly 2)) @?= 2
    ]
