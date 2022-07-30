module Data.Equality.Matching.Pattern where

import Data.Functor.Classes
import Data.String

import Data.Equality.Utils
import Data.Equality.Matching.Database

-- | @(~x + 0) --> BinOp Add (Var "~x") (ENode (Integer 0))@
-- @~x --> VariablePattern "~x"@
data Pattern lang
    = NonVariablePattern (lang (Pattern lang))
    | VariablePattern Var -- ^ Should be a >0 positive number

pat :: lang (Pattern lang) -> Pattern lang
pat = NonVariablePattern

instance Eq1 l => (Eq (Pattern l)) where
    (==) (NonVariablePattern a) (NonVariablePattern b) = liftEq (==) a b
    (==) (VariablePattern a) (VariablePattern b) = a == b 
    (==) _ _ = False

instance Ord1 l => (Ord (Pattern l)) where
    compare (VariablePattern _) (NonVariablePattern _) = LT
    compare (NonVariablePattern _) (VariablePattern _) = GT
    compare (VariablePattern a) (VariablePattern b) = compare a b
    compare (NonVariablePattern a) (NonVariablePattern b) = liftCompare compare a b

instance Show1 lang => Show (Pattern lang) where
    showsPrec _ (VariablePattern s) = showString (show s) -- ROMES:TODO don't ignore prec?
    showsPrec d (NonVariablePattern x) = liftShowsPrec showsPrec showList d x

instance IsString (Pattern lang) where
    fromString = VariablePattern . hashString

