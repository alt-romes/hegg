{-# LANGUAGE BangPatterns #-}

{- | Polynomial container for e-graphs.

A 'PolyMap' represents a multivariate polynomial as a multiset of multisets:

@
  3*x²*y + 2*x = { {x→2, y→1} → 3.0, {x→1} → 2.0 }
@

The outer map goes from monomial key (multiset of variable→exponent) to
coefficient.  This representation is canonical: identical polynomials always
produce the same 'PolyMap', regardless of how the expression was constructed.

When used inside an e-graph, the 'ClassId' keys in the monomial maps must be
re-canonicalized after merges (by applying @find@ to every key).

@since 0.6.0.0
-}
module Data.Equality.Graph.Poly (
    PolyMap (..),
    emptyPoly,
    constPoly,
    singletonPoly,
    addPoly,
    mulPoly,
    negatePoly,
    scalePoly,
    polySize,
    canonicalizePoly,
) where

import qualified Data.Map.Strict as Map
import Data.Equality.Graph.Classes.Id (ClassId)

-- | A polynomial over e-class IDs.
--
-- Outer map: monomial key → coefficient.
-- Inner map: ClassId → exponent (the multiset of factors).
--
-- Invariant: no entry with coefficient ≈ 0, no entry with exponent ≤ 0.
newtype PolyMap = PolyMap (Map.Map (Map.Map ClassId Int) Double)
    deriving (Eq, Ord, Show)

-- | The zero polynomial.
emptyPoly :: PolyMap
emptyPoly = PolyMap Map.empty

-- | A constant polynomial.
constPoly :: Double -> PolyMap
constPoly 0 = emptyPoly
constPoly c = PolyMap (Map.singleton Map.empty c)

-- | A single variable (e-class ID) with coefficient 1 and exponent 1.
singletonPoly :: ClassId -> PolyMap
singletonPoly cid = PolyMap (Map.singleton (Map.singleton cid 1) 1.0)

-- | Add two polynomials (merge monomials, sum coefficients).
addPoly :: PolyMap -> PolyMap -> PolyMap
addPoly (PolyMap a) (PolyMap b) = PolyMap (Map.unionWith (+) a b)

-- | Negate all coefficients.
negatePoly :: PolyMap -> PolyMap
negatePoly (PolyMap m) = PolyMap (Map.map negate m)

-- | Scale all coefficients by a constant.
scalePoly :: Double -> PolyMap -> PolyMap
scalePoly s (PolyMap m) = PolyMap (Map.map (* s) m)

-- | Multiply two polynomials (distribute and collect).
mulPoly :: PolyMap -> PolyMap -> PolyMap
mulPoly (PolyMap as) (PolyMap bs) = PolyMap $ Map.fromListWith (+)
    [ (Map.unionWith (+) ka kb, ca * cb)
    | (ka, ca) <- Map.toList as
    , (kb, cb) <- Map.toList bs
    ]

-- | Number of monomials.
polySize :: PolyMap -> Int
polySize (PolyMap m) = Map.size m

-- | Re-canonicalize a polynomial by applying a ClassId mapping function
-- to all keys.  After merges in the e-graph, some ClassIds may no longer
-- be canonical; this function updates them and merges any monomials
-- whose keys become identical.
canonicalizePoly :: (ClassId -> ClassId) -> PolyMap -> PolyMap
canonicalizePoly f (PolyMap m) = PolyMap $ Map.fromListWith (+)
    [ (canonKey, c)
    | (key, c) <- Map.toList m
    , let !canonKey = Map.fromListWith (+)
            [(f k, v) | (k, v) <- Map.toList key]
    ]
