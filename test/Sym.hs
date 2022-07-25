{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Sym where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.IntMap as IM
import qualified Data.Set    as S
import Data.String
import Data.Maybe (isJust)

import Data.Functor.Classes
import Control.Applicative (liftA2)
import Control.Monad (unless)

import Data.Equality.Graph
import Data.Equality.Graph.Lens
import Data.Equality.Matching
import Data.Equality.Saturation

data Expr a = Sym String
            | Const Double
            | UnOp  UOp a
            | BinOp BOp a a
            deriving ( Eq, Ord, Functor
                     , Foldable, Traversable
                     )

instance Eq1 Expr where
    liftEq eq a b = case (a, b) of
        (Sym x, Sym y) -> x == y
        (Const x, Const y) -> x == y
        (UnOp op x, UnOp op' y) -> op == op' && x `eq` y
        (BinOp op x y, BinOp op' x' y') -> op == op' && x `eq` x' && y `eq` y'
        _ -> False

instance Ord1 Expr where
    liftCompare cmp a b = case (a, b) of
        (Sym x, Sym y) -> compare x y
        (Const x, Const y) -> compare x y
        (UnOp op x, UnOp op' y) -> case compare op op' of
                                     EQ -> cmp x y
                                     o  -> o
        (BinOp op x y, BinOp op' x' y') -> case compare op op' of
                                             EQ -> case cmp x x' of
                                                     EQ -> cmp y y'
                                                     o  -> o
                                             o  -> o
        (x, y) -> compare (expIx x) (expIx y)
      where
        expIx :: Expr a -> Int
        expIx = \case
            Sym _ -> 1
            Const _ -> 2
            UnOp _ _ -> 3
            BinOp {} -> 4

instance Language Expr

data BOp = Add
         | Sub
         | Mul
         | Div
         | Pow
         | Diff
         | Integral
        deriving (Eq, Ord)

data UOp = Sin
         | Cos
         | Sqrt
         | Ln
    deriving (Eq, Ord)

instance Show BOp where
    show = \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Pow -> "^"
        Diff -> "d/d_"
        Integral -> "∫"

instance Show UOp where
    show = \case
        Sin -> "sin"
        Cos -> "cos"
        Sqrt -> "√"
        Ln -> "ln"

instance {-# OVERLAPPING #-} Show (Fix Expr) where
    show = cata $ \case
        BinOp Diff a b -> show Diff <> a <> " " <> b
        BinOp Integral a b -> show Integral <> a <> " " <> b
        BinOp op a b -> "(" <> a <> " " <> show op <> " " <> b <> ")"
        UnOp op a -> "( " <> show op <> "(" <> a <> ")" <> " )"
        Const x -> show x
        Sym x -> x

instance IsString (Fix Expr) where
    fromString = Fix . Sym

-- WARNING: Careful with negate implementation!!
-- The default implementation makes -1 = 0 - 1
-- Together with some rules this can be disastrous (infinitely)
instance Num (Fix Expr) where
    (+) a b = Fix (BinOp Add a b)
    (-) a b = Fix (BinOp Sub a b)
    (*) a b = Fix (BinOp Mul a b)
    fromInteger = Fix . Const . fromInteger
    negate = error "DONT USE" -- Fix . BinOp Mul (fromInteger $ -1)
    abs = error "abs"
    signum = error "signum"

instance Fractional (Fix Expr) where
    (/) a b = Fix (BinOp Div a b)
    fromRational = Fix . Const . fromRational

instance Show1 Expr where
    -- ROMES:TODO: Don't ignore precedence?
    liftShowsPrec sp _ d = \case
        BinOp Diff e1 e2 -> showString (show Diff) . sp d e1 . showString " " . sp d e2
        BinOp Integral e1 e2 -> showString (show Integral) . sp d e1 . showString " " . sp d e2
        BinOp op e1 e2 ->
            showString "(" . sp d e1 . showString (show op) . sp d e2 . showString ")"

        UnOp op e1 -> showString (show op) . sp d e1
        Const f -> showString (show f)
        Sym x -> showString x

symCost :: Expr Cost -> Cost
symCost = \case
    BinOp Pow e1 e2 -> e1 + e2 + 6
    BinOp Div e1 e2 -> e1 + e2 + 5
    BinOp Sub e1 e2 -> e1 + e2 + 4
    BinOp Mul e1 e2 -> e1 + e2 + 4
    BinOp Add e1 e2 -> e1 + e2 + 2
    BinOp Diff e1 e2 -> e1 + e2 + 500
    BinOp Integral e1 e2 -> e1 + e2 + 20000
    UnOp Sin e1 -> e1 + 20
    UnOp Cos e1 -> e1 + 20
    UnOp Sqrt e1 -> e1 + 30
    UnOp Ln   e1 -> e1 + 30
    Sym _ -> 1
    Const _ -> 1

instance Num (Pattern Expr) where
    (+) a b = NonVariablePattern $ BinOp Add a b
    (-) a b = NonVariablePattern $ BinOp Sub a b
    (*) a b = NonVariablePattern $ BinOp Mul a b
    fromInteger = NonVariablePattern . Const . fromInteger
    negate = error "DONT USE" -- NonVariablePattern. BinOp Mul (fromInteger $ -1)
    abs = error "abs"
    signum = error "signum"

instance Fractional (Pattern Expr) where
    (/) a b = NonVariablePattern $ BinOp Div a b
    fromRational = NonVariablePattern . Const . fromRational

powP :: Pattern Expr -> Pattern Expr -> Pattern Expr
powP a b = NonVariablePattern (BinOp Pow a b)

diffP :: Pattern Expr -> Pattern Expr -> Pattern Expr
diffP a b = NonVariablePattern (BinOp Diff a b)

intP :: Pattern Expr -> Pattern Expr -> Pattern Expr
intP a b = NonVariablePattern (BinOp Integral a b)

cosP :: Pattern Expr -> Pattern Expr
cosP a = NonVariablePattern (UnOp Cos a)

sinP :: Pattern Expr -> Pattern Expr
sinP a = NonVariablePattern (UnOp Sin a)

lnP :: Pattern Expr -> Pattern Expr
lnP a = NonVariablePattern (UnOp Ln a)


-- | Define analysis for the @Expr@ language over domain @Maybe Double@ for
-- constant folding
instance Analysis Expr where
    type Domain Expr = Maybe Double

    makeA (Node e) egr = evalConstant ((\c -> egr^._class c._data) <$> e)

    -- joinA = (<|>)
    joinA ma mb = do
        a <- ma
        b <- mb
        -- this assertion only seemed to be triggering when using bogus
        -- constant assignments for "Fold all classes with x:=c"
        -- 0 bug found by property checking
        !_ <- unless (a == b || (a == 0 && b == (-0)) || (a == (-0) && b == 0)) (error "Merged non-equal constants!")
        return a

    modifyA i egr =
        case egr ^._class i._data of
          Nothing -> egr
          Just d  -> snd $ runEGS egr $ do

            -- Add constant as e-node
            new_c <- represent (Fix $ Const d)
            _ <- merge i new_c

            -- Prune all except leaf e-nodes
            modify (_class i._nodes %~ S.filter (null . children))



evalConstant :: Expr (Maybe Double) -> Maybe Double
evalConstant = \case
    -- Exception: Negative exponent: BinOp Pow e1 e2 -> liftA2 (^) e1 (round <$> e2 :: Maybe Integer)
    BinOp Div e1 e2 -> liftA2 (/) e1 e2
    BinOp Sub e1 e2 -> liftA2 (-) e1 e2
    BinOp Mul e1 e2 -> liftA2 (*) e1 e2
    BinOp Add e1 e2 -> liftA2 (+) e1 e2
    BinOp Pow _ _ -> Nothing
    BinOp Diff _ _ -> Nothing
    BinOp Integral _ _ -> Nothing
    UnOp Sin e1 -> sin <$> e1
    UnOp Cos e1 -> cos <$> e1
    UnOp Sqrt e1 -> sqrt <$> e1
    UnOp Ln   _  -> Nothing
    Sym _ -> Nothing
    Const x -> Just x
    
unsafeGetSubst :: Pattern Expr -> Subst -> ClassId
unsafeGetSubst (NonVariablePattern _) _ = error "unsafeGetSubst: NonVariablePattern; expecting VariablePattern"
unsafeGetSubst (VariablePattern v) subst = case IM.lookup v subst of
      Nothing -> error "Searching for non existent bound var in conditional"
      Just class_id -> class_id

is_not_zero :: Pattern Expr -> RewriteCondition Expr
is_not_zero v subst egr =
    egr^._class (unsafeGetSubst v subst)._data /= Just 0

is_sym :: Pattern Expr -> RewriteCondition Expr
is_sym v subst egr =
    any ((\case (Sym _) -> True; _ -> False) . unNode) (egr^._class (unsafeGetSubst v subst)._nodes)

is_const :: Pattern Expr -> RewriteCondition Expr
is_const v subst egr =
    isJust (egr^._class (unsafeGetSubst v subst)._data)

is_const_or_distinct_var :: Pattern Expr -> Pattern Expr -> RewriteCondition Expr
is_const_or_distinct_var v w subst egr =
    let v' = unsafeGetSubst v subst
        w' = unsafeGetSubst w subst
     in (eClassId (egr^._class v') /= eClassId (egr^._class w'))
        && (isJust (egr^._class v'._data)
            || any ((\case (Sym _) -> True; _ -> False) . unNode) (egr^._class v'._nodes))

rewrites :: [Rewrite Expr]
rewrites =
    [ "a"+"b" := "b"+"a" -- comm add
    , "a"*"b" := "b"*"a" -- comm mul
    , "a"+("b"+"c") := ("a"+"b")+"c" -- assoc add
    , "a"*("b"*"c") := ("a"*"b")*"c" -- assoc mul

    , "a"-"b" := "a"+(fromInteger (-1) * "b") -- sub cannon
    , "a"/"b" := "a"*powP "b" (fromInteger $ -1) :| is_not_zero "b" -- div cannon

    -- identities
    , "a"+0 := "a"
    , "a"*0 := 0
    , "a"*1 := "a"

    -- TODO This causes many problems
    -- , "a" := "a"+0

    -- This already works
    , "a" := "a"*1

    , "a"-"a" := 0 -- cancel sub
    , "a"/"a" := 1 :| is_not_zero "a" -- cancel div

    , "a"*("b"+"c") := ("a"*"b")+("a"*"c") -- distribute
    , ("a"*"b")+("a"*"c") := "a"*("b"+"c") -- factor

    , powP "a" "b"*powP "a" "c" := powP "a" ("b" + "c") -- pow mul
    , powP "a" 0 := 1 :| is_not_zero "a"
    , powP "a" 1 := "a"
    , powP "a" 2 := "a"*"a"
    , powP "a" (fromInteger $ -1) := 1/"a" :| is_not_zero "a"

    , "x"*(1/"x") := 1 :| is_not_zero "x"

    , diffP "x" "x" := 1 :| is_sym "x"
    , diffP "x" "c" := 0 :| is_sym "x" :| is_const_or_distinct_var "c" "x"

    , diffP "x" ("a" + "b") := diffP "x" "a" + diffP "x" "b"
    , diffP "x" ("a" * "b") := ("a"*diffP "x" "b") + ("b"*diffP "x" "a")

    , diffP "x" (sinP "x") := cosP "x"
    , diffP "x" (cosP "x") := fromInteger (-1) * sinP "x"

    , diffP "x" (lnP "x") := 1/"x" :| is_not_zero "x"

    -- diff-power
    , diffP "x" (powP "f" "g") := powP "f" "g" * ((diffP "x" "f" * ("g" / "f")) +
        (diffP "x" "g" * lnP "f")) :| is_not_zero "f" :| is_not_zero "g"

    -- i-one
    , intP 1 "x" := "x"

    -- i power const
    , intP (powP "x" "c") "x" := (/) (powP "x" ((+) "c" 1)) ((+) "c" 1) :| is_const "c"

    , intP (cosP "x") "x" := sinP "x"
    , intP (sinP "x") "x" := fromInteger (-1)*cosP "x"

    , intP ("f" + "g") "x" := intP "f" "x" + intP "g" "x"

    -- , intP ("f" - "g") "x" := intP "f" "x" - intP "g" "x"

    , intP ("a" * "b") "x" := (-) ((*) "a" (intP "b" "x")) (intP ((*) (diffP "x" "a") (intP "b" "x")) "x")

    -- Additional ad-hoc: because of negate representations?
    , "a"-(fromInteger (-1)*"b") := "a"+"b"

    ]

rewrite :: Fix Expr -> Fix Expr
rewrite e = fst $ equalitySaturation e rewrites symCost

symTests :: TestTree
symTests = testGroup "Symbolic"
    [ testCase "(a*2)/2 = a (custom rules)" $
        fst (equalitySaturation (("a"*2)/2) [ ("x"*"y")/"z" := "x"*("y"/"z")
                                            , "y"/"y" := 1
                                            , "x"*1 := "x"] symCost) @?= "a"

    , testCase "(a/2)*2 = a (all rules)" $
        rewrite (("a"/2)*2) @?= "a"

    , testCase "(a+a)/2 = a (extra rules)" $
        rewrite (("a"+"a")/2) @?= "a"

    , testCase "x/y (custom rules)" $
        -- without backoff scheduler this will loop forever
        fst (equalitySaturation
                ("x"/"y")

                [ "x"/"y" := "x"*(1/"y")
                , "x"*("y"*"z") := ("x"*"y")*"z"
                ]

                symCost) @?= ("x"/"y")

    , testCase "0+1 = 1 (all rules)" $
        fst (equalitySaturation (0+1) rewrites symCost)   @?= 1

    , testCase "b*(1/b) = 1 (custom rules)" $
        fst (equalitySaturation ("b"*(1/"b")) [ "a"*(1/"a") := 1 ] symCost) @?= 1

    , testCase "1+1=2 (constant folding)" $
        fst (equalitySaturation (1+1) [] symCost) @?= 2

    , testCase "a*(2-1) (1 rule + constant folding)" $
        fst (equalitySaturation ("a" * (2-1)) ["x"*1:="x"] symCost) @?= "a"

    , testCase "1+a*(2-1) = 1+a (all + constant folding)" $
        rewrite (1+("a"*(2-1))) @?= (1+"a")

    , testCase "1+a*(2-1) = 1+a (all + constant f.)" $
        rewrite (fromInteger(-3)+fromInteger(-3)-6) @?= Fix (Const $ -12)

    , testCase "1+a-a*(2-1) = 1 (all + constant f.)" $
        rewrite (1 + "a" - "a"*(2-1)) @?= 1

    , testCase "1+(a-a*(2-1)) = 1 (all + constant f.)" $
        rewrite ("a" - "a"*(4-1)) @?= "a"*(Fix . Const $ -2)

    , testCase "x + x + x + x = 4*x" $
        rewrite ("a"+"a"+"a"+"a") @?= "a"*4

    , testCase "math powers" $
        rewrite (Fix (BinOp Pow 2 "x")*Fix (BinOp Pow 2 "y")) @?= Fix (BinOp Pow 2 ("x" + "y"))

    , testCase "d1" $
        rewrite (Fix $ BinOp Diff "a" "a") @?= 1

    , testCase "d2" $
        rewrite (Fix $ BinOp Diff "a" "b") @?= 0

    , testCase "d3" $
        rewrite (Fix $ BinOp Diff "x" (1 + 2*"x")) @?= 2

    , testCase "d4" $
        rewrite (Fix $ BinOp Diff "x" (1 + "y"*"x")) @?= "y"

    , testCase "d5" $
        rewrite (Fix $ BinOp Diff "x" (Fix $ UnOp Ln "x")) @?= 1/"x"

    , testCase "i1" $
        rewrite (Fix $ BinOp Integral 1 "x") @?= "x"

    , testCase "i2" $
        rewrite (Fix $ BinOp Integral (Fix $ UnOp Cos "x") "x") @?= Fix (UnOp Sin "x")

    , testCase "i3" $
        rewrite (Fix $ BinOp Integral (Fix $ BinOp Pow "x" 1) "x") @?= "x"*("x"*0.5)

    , testCase "i4" $
        rewrite (_i ((*) "x" (_cos "x")) "x") @?= (+) (_cos "x") ((*) "x" (_sin "x"))

    , testCase "i5" $
        rewrite (_i ((*) (_cos "x") "x") "x") @?= (+) (_cos "x") ((*) "x" (_sin "x"))

    -- TODO: How does this even work ?
    , testCase "i6" $
        rewrite (_i (_ln "x") "x") @?= "x"*(_ln "x" + fromInteger(-1))

    ]

_i :: Fix Expr -> Fix Expr -> Fix Expr
_i a b = Fix (BinOp Integral a b)
_ln, _cos, _sin :: Fix Expr -> Fix Expr
_ln a = Fix (UnOp Ln a)
_cos a = Fix (UnOp Cos a)
_sin a = Fix (UnOp Sin a)
