{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module T3 where

-- Some e-graph unit tests

import Prelude hiding (not)

-- import Test.Tasty.HUnit
import Data.Equality.Graph
import Data.Equality.Utils
import qualified Data.Equality.Graph.Monad as EGM

data Lang a = And a a
            | Or a a
            | Not a
            | ToElim a
            | Sym Int
            deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

main :: IO ()
main = do
  let _ = EGM.egraph @Lang @() $ do
        id1 <- EGM.represent (Fix (Sym 1))
        id2 <- EGM.represent (Fix (Not (Fix (Not (Fix (Sym 1))))))
        id3 <- EGM.represent (Fix (Sym 2))
        a1  <- EGM.add (Node (And id3 id1))
        a2  <- EGM.add (Node (And id3 id2))
        _ <- EGM.merge a1 a2
        EGM.rebuild -- even rebuilding this fails...
        return (id1,id2)

  -- The children should now be in the same e-class?
  --
  -- Turns out, they don't. So this test should actually be the other way
  -- around (we do not learn s1 ~= s2 from merging a1 and a2)
  --
  -- A counter example:
  -- Consider `f` that returns its second argument (f _ x = x):
  -- so f(a,c) = f(b,c), but a != b
  --
  -- find s1 eg @=? find s2 eg

  return ()

