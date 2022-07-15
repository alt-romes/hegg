{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Sym where

import Debug.Trace
import Test.Tasty
import Test.Tasty.HUnit

import Data.String

import Data.Functor.Classes
import Control.Applicative (liftA2)

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
         | Negate
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
        Negate -> "-"

instance {-# OVERLAPPING #-} Show (Fix Expr) where
    show = foldFix $ \case
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
    negate = Fix . UnOp Negate
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
    BinOp Pow e1 e2 -> e1 + e2 + 20
    BinOp Div e1 e2 -> e1 + e2 + 5
    BinOp Sub e1 e2 -> e1 + e2 + 5
    BinOp Mul e1 e2 -> e1 + e2 + 4
    BinOp Add e1 e2 -> e1 + e2 + 2
    BinOp Diff e1 e2 -> e1 + e2 + 100
    BinOp Integral e1 e2 -> e1 + e2 + 200
    UnOp Sin e1 -> e1 + 20
    UnOp Cos e1 -> e1 + 20
    UnOp Sqrt e1 -> e1 + 30
    UnOp Ln   e1 -> e1 + 30
    UnOp Negate e1 -> e1 + 5
    Sym _ -> 1
    Const _ -> 1

instance Num (Pattern Expr) where
    (+) a b = NonVariablePattern $ BinOp Add a b
    (-) a b = NonVariablePattern $ BinOp Sub a b
    (*) a b = NonVariablePattern $ BinOp Mul a b
    negate x = NonVariablePattern $ UnOp Negate x
    fromInteger = NonVariablePattern . Const . fromInteger
    abs = error "abs"
    signum = error "signum"

instance Fractional (Pattern Expr) where
    (/) a b = NonVariablePattern $ BinOp Div a b
    fromRational = NonVariablePattern . Const . fromRational

pattern PowP :: Pattern Expr -> Pattern Expr -> Pattern Expr
pattern PowP a b = NonVariablePattern (BinOp Pow a b)

pattern DiffP :: Pattern Expr -> Pattern Expr -> Pattern Expr
pattern DiffP a b = NonVariablePattern (BinOp Diff a b)

pattern CosP :: Pattern Expr -> Pattern Expr
pattern CosP a = NonVariablePattern (UnOp Cos a)

pattern SinP :: Pattern Expr -> Pattern Expr
pattern SinP a = NonVariablePattern (UnOp Sin a)

pattern LnP :: Pattern Expr -> Pattern Expr
pattern LnP a = NonVariablePattern (UnOp Ln a)


-- | Define analysis for the @Expr@ language over domain @Maybe Double@ for
-- constant folding
instance Analysis Expr where
    type Domain Expr = Maybe Double

    makeA (Node e) egr = evalConstant ((\c -> egr^._class c._data) <$> e)

    -- joinA = (<|>)
    joinA ma mb = do
        a <- ma
        _ <- mb
        -- this assertion only seemed to be triggering when using bogus
        -- constant assignments for "Fold all classes with x:=c"
        -- !_ <- unless (a == b) (error "assert failed!")
        return a

    modifyA i egr =
        case egr ^._class i._data of
          Nothing -> egr
          Just d  -> snd $ runEGS egr $ do

            -- Add constant as e-node
            new_c <- represent (Fix $ Const d)
            merge i new_c

            -- Prune all except leaf e-nodes
            -- modifyClasses (IM.modify (\E) ji)



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
    UnOp Negate e1 -> (\e -> -e) <$> e1
    Sym _ -> Nothing
    Const x -> Just x
    


rewrites :: [Rewrite Expr]
rewrites =
    [ "x"+"y" := "y"+"x" -- comm add
    , "x"*"y" := "y"*"x" -- comm mul
    , "x"+("y"+"z") := ("x"+"y")+"z" -- assoc add
    , "x"*("y"*"z") := ("x"*"y")*"z" -- assoc mul

    , "x"-"y" := "x"+((-1)*"y") -- sub cannon
    , "x"/"y" := "x"*PowP "y" (-1) -- div cannon

    -- identities
    , "x"+0 := "x"
    , "x"*0 := 0
    , "x"*1 := "x"

    -- TODO: This collapses all classes...
    -- , "x" := "x"+0
    -- , "x" := "x"*1

    , "a"-"a" := 1 -- cancel sub
    , "a"/"a" := 1 -- cancel div

    , "x"*("y"+"z") := ("x"*"y")+("x"*"z") -- distribute
    , ("x"*"y")+("x"*"z") := "x"*("y"+"z") -- factor

    , PowP "a" "b"*PowP "a" "c" := PowP "a" ("b" + "c") -- pow mul
    , PowP "a" 0 := 1
    , PowP "a" 1 := "a"
    , PowP "a" 2 := "a"*"a"
    , PowP "a" (-1) := 1/"a"

    , "x"*(1/"x") := 1

    , DiffP "x" "x" := 1
    , DiffP "x" "y" := 0

    , DiffP "x" ("a" + "b") := DiffP "x" "a" + DiffP "x" "b"
    , DiffP "x" ("a" * "b") := ("a"*DiffP "x" "b") + ("b"*DiffP "x" "a")

    , DiffP "x" (SinP "x") := CosP "x"
    , DiffP "x" (CosP "x") := (-1)*SinP "x"

    , DiffP "x" (LnP "x") := 1/"x"

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
        fst (equalitySaturation (("a"+"a")/2) (["x"+"x" := 2*"x"] <> rewrites) symCost) @?= "a"

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

    -- , testCase "1+1=2 (constant folding)" $
    --     rewrite (1 + ("a" - ("a"*(2-1)))) @?= 1

    -- , testCase "d1" $
    --     rewrite (Fix $ Diff "a" "a") @?= 1

    -- , testCase "d2" $
    --     rewrite (Fix $ Diff "a" "b") @?= 0

    -- , testCase "d3" $
    --     rewrite (Fix $ Diff "x" (1 + 2*"x")) @?= 2

    ]
