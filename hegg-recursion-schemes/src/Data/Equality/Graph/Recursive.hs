{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Equality.Graph.Recursive where

import qualified Data.Equality.Graph as EG
import qualified Data.Equality.Analysis as EG
import qualified Data.Equality.Utils as EG

import Unsafe.Coerce (unsafeCoerce)
import Data.Fix
import Data.Functor.Foldable

type Language language = EG.Language (Base language)
type Analysis domain language = EG.Analysis domain (Base language)
type EGraph analysis language = EG.EGraph analysis (Base language)

emptyEGraph :: Language l => EGraph a l
emptyEGraph = EG.emptyEGraph

represent :: forall a lang. (Analysis a lang, Language lang, Recursive lang) => lang -> EGraph a lang -> (EG.ClassId, EGraph a lang) 
represent l = EG.represent (coerceFix $ refix l)


coerceFix :: Fix f -> EG.Fix f
coerceFix = unsafeCoerce

