{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Computed where

-- Tests computed rewrites

import Prelude hiding (not)

import Test.Tasty.HUnit
import Data.Equality.Graph.Nodes (ENode(..))
import Data.Equality.Matching (Pattern, pat)
import Data.Equality.Extraction
import Data.Equality.Saturation.Rewrites
import Data.Equality.Saturation
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Lang a = Add a a
            | Lit Int
            deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

x, y :: Pattern Lang
x = "x"
y = "y"

foldAddRule :: Rewrite () Lang
foldAddRule = pat (x `Add` y) :=> f
  where
    f :: RewriteFun () Lang
    f ctx = do
        xn <- isLit =<< M.lookup "x" ctx
        yn <- isLit =<< M.lookup "y" ctx
        return $ Fix $ Lit (xn + yn)

    isLit :: MatchInfo () Lang -> Maybe Int
    isLit (MatchInfo _ nodes) =
        listToMaybe [ n | Node (Lit n) <- S.toList nodes ]

rules :: [Rewrite () Lang]
rules = [foldAddRule]

main :: IO ()
main = do
  fst (equalitySaturation (Fix $ (Fix $ Lit 1) `Add` (Fix $ Lit 1)) rules depthCost) @?= Fix (Lit 2)
  pure ()

