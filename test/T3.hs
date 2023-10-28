{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module T3 where

-- Some e-graph unit tests

import Prelude hiding (not)

import Test.Tasty.HUnit
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
  let ((s1,s2), eg) = EGM.egraph @Lang @() $ do
        id1 <- EGM.represent (Fix (Sym 1))
        id2 <- EGM.represent (Fix (Not (Fix (Not (Fix (Sym 1))))))
        id3 <- EGM.represent (Fix (Sym 2))
        a1  <- EGM.add (Node (And id3 id1))
        a2  <- EGM.add (Node (And id3 id2))
        EGM.merge a1 a2
        -- EGM.rebuild
        return (id1,id2)

  -- The children should now be in the same e-class
  find s1 eg @=? find s2 eg

  return ()

