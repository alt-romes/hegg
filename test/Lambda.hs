module Lambda where

import Data.Fix

data Lambda a
    = Bool Bool
    | Num Int
    | Var String
    | Add a a
    | Eq a a
    | App a a
    | Lam a a
    | Let a a a
    | LFix a a
    | If a a
    | Symbol String


-- eval :: Fix Lambda -> _
-- eval = foldFix $ \case
--     Num

