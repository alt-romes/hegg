{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
   Composable 'Analysis' instances.

   They are currently defined as orphans in this module -- defining them next to
   the class causes multiple cyclic dependencies problems. I'm unsure which is
   less bad, the orphan instances or the cyclic dependencies
 -}
module Data.Equality.Analysis.Instances where

import Control.Arrow ((***))

import Data.Equality.Utils

import Data.Equality.Language
import Data.Equality.Analysis
import Data.Equality.Graph.Classes

-- This instance is only well behaved for both analysis, where 'modifyA' is
-- called @m1@ and @m2@ respectively, if @m1@ and @m2@ commute.
--
-- That is, @m1@ and @m2@ must satisfy the following law:
-- @
-- m1 . m2 = m2 . m1
-- @
--
-- Here is a simple criterion that should suffice though. If:
--  * The modify function only depends on the analysis value, and
--  * The modify function doesn't change the analysis value
-- Then any two such functions commute.
instance (Language l, Analysis a l, Analysis b l) => Analysis (a, b) l where
  type Domain (a,b) l = (Domain a l, Domain b l)

  makeA :: l (Domain a l, Domain b l) -> (Domain a l, Domain b l)
  makeA g = (makeA @a (fst <$> g), makeA @b (snd <$> g))

  joinA :: Domain (a,b) l -> Domain (a,b) l -> Domain (a,b) l
  joinA (x,y) = joinA @a @l x *** joinA @b @l y

  modifyA :: EClass (a, b) l -> (EClass (a, b) l, [Fix l])
  modifyA c =
    let (ca, la) = modifyA @a (c { eClassData = fst (eClassData c) })
        (cb, lb) = modifyA @b (c { eClassData = snd (eClassData c) })
     in ( EClass (eClassId c) (eClassNodes ca <> eClassNodes cb) (eClassData ca, eClassData cb) (eClassParents ca <> eClassParents cb)
        , la <> lb
        )

--   modifyA :: ClassId -> EGraph (a, b) l -> EGraph (a, b) l
--   modifyA i g =
--     let g_a = g { classes = fmap (\c -> c { eClassData = fst (eClassData c) }) (classes g) }
--         g_b = g { classes = fmap (\c -> c { eClassData = snd (eClassData c) }) (classes g) }
--         r_a = modifyA @a @l i g_a
--         r_b = modifyA @b @l i g_b
--         d_a = r_a ^. _class i . _data
--         d_b = r_b ^. _class i . _data
--      in _

