{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module Data.Equality.Saturation
    ( module Data.Equality.Saturation
    , Fix(..), foldFix, unfoldFix
    , Cost
    ) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Data.Functor.Classes
import Data.Traversable
import Control.Monad
import Control.Monad.State

import Data.Fix

import Data.Equality.Graph
import Data.Equality.Matching
import Data.Equality.Extraction

data Rewrite lang = PatternAST lang := PatternAST lang
infix 3 :=

equalitySaturation :: forall lang. (Show1 lang, Show (lang (PatternAST lang)), Ord (lang ()), Ord (ENode lang), Traversable lang) 
                   => Fix lang -> [Rewrite lang] -> (lang Cost -> Cost) -> (Fix lang, EGraph lang)
equalitySaturation exp rewrites cost = runEGS emptyEGraph $ do

    -- Represent expression as an e-graph
    origClass <- represent exp

    -- Run equality saturation
    equalitySaturation' 7 -- Stop after X iterations

    -- Extract best solution from the e-class of the original expression
    g <- get
    return $ extractBest g cost origClass

      where

        equalitySaturation' :: Int -> EGS lang ()
        equalitySaturation' 0 = return ()
        equalitySaturation' i = do

            (EGraph { memo = beforeMemo, classes = beforeClasses }) <- get

            -- Read-only phase, invariants are preserved
            matches <- join <$> forM rewrites \(lhs := rhs) -> do 
                    map (rhs,) <$> ematchM lhs -- Add rewrite right hand side to the e-match substitutions

            -- Write-only phase, temporarily break invariants
            forM_ matches \case
                (VariablePattern v, (subst, eclass)) -> do
                    -- rhs is equal to right hand side, simply merge class where lhs
                    -- pattern was found (@eclass@) and the eclass the pattern variable
                    -- matched (@lookup v subst@)
                    case lookup v subst of
                      Nothing -> error "impossible: couldn't find v in subst"
                      Just i  -> merge i eclass
                (NonVariablePattern rhs, (subst, eclass)) -> do
                    -- rhs is (at the top level) a non-variable pattern, so substitute
                    -- all pattern variables in the pattern and create a new e-node (and
                    -- e-class that represents it), then merge the e-class of the
                    -- substituted rhs with the class that matched the left hand side
                    eclass' <- reprPat subst rhs
                    merge eclass eclass'

            -- Restore the invariants once per iteration
            rebuild
            
            (EGraph { memo = afterMemo, classes = afterClasses }) <- get

            -- ROMES:TODO: Node limit...
            -- ROMES:TODO: Actual Timeout... not just iteration timeout
            -- ROMES:TODO Better saturation (see Runner)
            -- Apply rewrites until saturated or ROMES:TODO: timeout
            unless (M.size afterMemo == M.size beforeMemo && IM.size afterClasses == IM.size beforeClasses)
                (equalitySaturation' (i-1))


        -- | Represent a pattern in the e-graph a pattern given substitions
        reprPat :: (Ord (ENode lang), Traversable lang)
                => Subst -> lang (PatternAST lang) -> EGS lang ClassId
        reprPat subst = add <=< traverse \case
            VariablePattern v ->
                case lookup v subst of
                    Nothing -> error "impossible: couldn't find v in subst?"
                    Just i  -> return i
            NonVariablePattern p -> reprPat subst p

