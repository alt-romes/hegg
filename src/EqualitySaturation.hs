{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module EqualitySaturation where

import Data.Traversable

import Control.Monad

import EGraph
import EMatching

data Rewrite lang = PatternAST lang := PatternAST lang

infix 3 :=

equalitySaturation :: (Ord (lang ()), Ord (ENode lang), Traversable lang)
                   => ERepr exp lang
                   => exp -> [Rewrite lang] -> EGraph lang
equalitySaturation exp rewrites = snd $ runEGS emptyEGraph $ do

    -- Represent expression as an e-graph
    represent exp

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

    where
        -- | Represent a pattern in the e-graph a pattern given substitions
        reprPat :: (Ord (ENode lang), Traversable lang)
                => Subst -> lang (PatternAST lang) -> EGS lang ClassId
        reprPat subst = add <=< traverse \case
            VariablePattern v ->
                case lookup v subst of
                    Nothing -> error "impossible: couldn't find v in subst?"
                    Just i  -> return i
            NonVariablePattern p -> reprPat subst p


            
