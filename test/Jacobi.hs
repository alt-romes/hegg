{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Jacobi where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.IntMap.Strict as IM
import qualified Data.Set    as S
import Data.String
import Data.List (sort)
import Data.Maybe (isJust)

import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving

import qualified Data.Foldable as F

import Control.Applicative (liftA2)
import Control.Monad (unless)

import Data.Equality.Graph.Lens
import Data.Equality.Graph
import Data.Equality.Extraction
import Data.Equality.Analysis
import Data.Equality.Matching
import Data.Equality.Matching.Database
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler
import Numeric.GSL.Special(elljac_e)

data Expr a = Sym   !String
            | Const !Double
            | Sum [a]
            | Prod [a]
            | UnOp  !UOp !a
            | BinOp !BOp !a !a
            deriving ( Eq, Ord, Show, Functor
                     , Foldable, Traversable
                     )
data BOp = Pow
         | Sn   -- ^ Glaisher notation for Jacobi's sine of the amplitude
         | Cn   -- ^ Glaisher notation for Jacobi's cosine of the amplitude
         | Dn   -- ^ Glaisher notation for Jacobi's derivative/delta of the amplitude
         | Diff
         | Integral
        deriving (Eq, Ord, Show)

data UOp = Sin
         | Cos
         | Sinh
         | Cosh
         | Sqrt
         | Ln
         deriving (Eq, Ord, Show)

deriveEq1 ''Expr
deriveOrd1 ''Expr
deriveShow1 ''Expr

instance IsString (Fix Expr) where
    fromString = Fix . Sym

-- TODO: There should be a way to do this with TH.
-- This Ord instance doesn't seem to get derived.
instance Ord (Fix Expr) where
    Fix (Sym v) `compare` e = case e of
      Fix (Sym v') -> v `compare` v'
      _ -> LT
    Fix (Const x) `compare` e = case e of
      Fix (Sym _) -> GT
      Fix (Const y) -> x `compare` y
      _ -> LT
    Fix (Sum es) `compare` e = case e of
      Fix (Sym _) -> GT
      Fix (Const _) -> GT
      Fix (Sum es') -> es `compare` es'
      _ -> LT
    Fix (Prod es) `compare` e = case e of
      Fix (Sym _) -> GT
      Fix (Const _) -> GT
      Fix (Sum _) -> GT
      Fix (Prod es') -> es `compare` es'
      _ -> LT
    Fix (UnOp o x) `compare` e = case e of
      Fix (Sym _) -> GT
      Fix (Const _) -> GT
      Fix (Sum _) -> GT
      Fix (Prod _) -> GT
      Fix (UnOp o' y) -> (o, x) `compare` (o', y)
      _ -> LT
    Fix (BinOp o x y) `compare` e = case e of
      Fix (Sym _) -> GT
      Fix (Const _) -> GT
      Fix (Sum _) -> GT
      Fix (Prod _) -> GT
      Fix (UnOp _ _) -> GT
      Fix (BinOp o' x' y') -> (o, x, y) `compare` (o', x', y')

instance Num (Fix Expr) where
    (+) a b = case (a, b) of
      (Fix (Sum as), Fix (Sum bs)) -> Fix . Sum . sort $ as ++ bs
      (Fix (Sum as), _) -> Fix . Sum . sort $ b : as
      (_, Fix (Sum bs)) -> Fix . Sum . sort $ a : bs
      _ -> Fix . Sum . sort $ [a,b]
    (-) a b = a + negate b
    (*) a b = case (a, b) of
      (Fix (Prod as), Fix (Prod bs)) -> Fix . Prod . sort $ as ++ bs
      (Fix (Prod as), _) -> Fix . Prod . sort $ b : as
      (_, Fix (Prod bs)) -> Fix . Prod . sort $ a : bs
      _ -> Fix . Prod . sort $ [a,b]
    fromInteger = Fix . Const . fromInteger
    negate = (*) . fromInteger $ -1
    abs    = error "abs"
    signum = error "signum"

instance Fractional (Fix Expr) where
    (/) a b = a * Fix (BinOp Pow b . fromInteger $ -1)
    fromRational = Fix . Const . fromRational

-- Sum-of-products preference might want a different recursion scheme.
symCost :: CostFunction Expr Int
symCost = \case
    BinOp Sn e1 e2 -> e1 + e2 + 50
    BinOp Cn e1 e2 -> e1 + e2 + 50
    BinOp Dn e1 e2 -> e1 + e2 + 50
    BinOp Pow e1 e2 -> e1 + e2 + 1
    Sum es -> sum es + length es + 5
    Prod es -> sum es + length es + 10
    BinOp Diff e1 e2 -> e1 + e2 + 500
    BinOp Integral e1 e2 -> e1 + e2 + 20000
    UnOp Sin e1 -> e1 + 20
    UnOp Cos e1 -> e1 + 20
    UnOp Sinh e1 -> e1 + 20
    UnOp Cosh e1 -> e1 + 20
    UnOp Sqrt e1 -> e1 + 30
    UnOp Ln   e1 -> e1 + 30
    Sym _ -> 1
    Const _ -> 1

-- This Num instance for Pattern Expr may not reflect very well what's
-- needed to successfully describe desired structure.
instance Num (Pattern Expr) where
    (+) a b = NonVariablePattern $ Sum [a,b]
    (-) a b = NonVariablePattern $ Sum [a, NonVariablePattern $ Prod [fromInteger (-1), b]]
    (*) a b = NonVariablePattern $ Prod [a,b]
    fromInteger = NonVariablePattern . Const . fromInteger
    negate = error "DONT USE" -- NonVariablePattern. BinOp Mul (fromInteger $ -1)
    abs = error "abs"
    signum = error "signum"

instance Fractional (Pattern Expr) where
    (/) a b = NonVariablePattern $ Prod [a, NonVariablePattern $ BinOp Pow b (fromInteger (-1))]
    fromRational = NonVariablePattern . Const . fromRational

-- | Define analysis for the @Expr@ language over domain @Maybe Double@ for
-- constant folding
instance Analysis (Maybe Double) Expr where

    makeA = evalConstant

    -- joinA = (<|>)
    joinA ma mb = do
        a <- ma
        b <- mb
        -- this assertion only seemed to be triggering when using bogus
        -- constant assignments for "Fold all classes with x:=c"
        -- 0 bug found by property checking
        !_ <- unless (a == b || (a == 0 && b == (-0)) || (a == (-0) && b == 0)) (error "Merged non-equal constants!")
        return a

    modifyA cl = case cl^._data of
                 Nothing -> (cl, [])
                 Just d -> ((_nodes %~ S.filter (F.null .unNode)) cl, [Fix (Const d)])

    --         -- Add constant as e-node
    --         new_c <- represent (Fix $ Const d)
    --         _     <- GM.merge i new_c

    --         -- Prune all except leaf e-nodes
    --         modify (_class i._nodes %~ S.filter (F.null . unNode))



evalConstant :: Expr (Maybe Double) -> Maybe Double
evalConstant = \case
    -- Exception: Negative exponent: BinOp Pow e1 e2 -> liftA2 (^) e1 (round <$> e2 :: Maybe Integer)
    Sum [] -> Just 0
    Sum es@(_:_) -> foldr1 (liftA2 (+)) es
    Prod [] -> Just 1
    Prod es@(_:_) -> foldr1 (liftA2 (*)) es
    BinOp Pow e1 e2 -> liftA2 (**) e1 e2
    BinOp Sn e1 e2 -> fmap (\(x,_,_) -> x) $ liftA2 elljac_e e1 e2
    BinOp Cn e1 e2 -> fmap (\(_,x,_) -> x) $ liftA2 elljac_e e1 e2
    BinOp Dn e1 e2 -> fmap (\(_,_,x) -> x) $ liftA2 elljac_e e1 e2
    BinOp Diff _ _ -> Nothing
    BinOp Integral _ _ -> Nothing
    UnOp Sin e1 -> sin <$> e1
    UnOp Cos e1 -> cos <$> e1
    UnOp Sinh e1 -> sinh <$> e1
    UnOp Cosh e1 -> cosh <$> e1
    UnOp Sqrt e1 -> sqrt <$> e1
    UnOp Ln   _  -> Nothing
    Sym _ -> Nothing
    Const x -> Just x

unsafeGetSubst :: Pattern Expr -> Subst -> ClassId
unsafeGetSubst (NonVariablePattern _) _ = error "unsafeGetSubst: NonVariablePattern; expecting VariablePattern"
unsafeGetSubst (VariablePattern v) subst = case IM.lookup v subst of
      Nothing -> error "Searching for non existent bound var in conditional"
      Just class_id -> class_id

is_not_zero :: Pattern Expr -> RewriteCondition (Maybe Double) Expr
is_not_zero v subst egr =
    egr^._class (unsafeGetSubst v subst)._data /= Just 0

is_int :: Pattern Expr -> RewriteCondition (Maybe Double) Expr
is_int v subst egr =
    case egr^._class (unsafeGetSubst v subst)._data of
      Just x -> snd (properFraction x :: (Int, Double)) == 0
      Nothing -> False

is_positive :: Pattern Expr -> RewriteCondition (Maybe Double) Expr
is_positive v subst egr =
    case egr^._class (unsafeGetSubst v subst)._data of
      Just x -> x > 0
      Nothing -> False

is_sym :: Pattern Expr -> RewriteCondition (Maybe Double) Expr
is_sym v subst egr =
    any ((\case (Sym _) -> True; _ -> False) . unNode) (egr^._class (unsafeGetSubst v subst)._nodes)

is_const :: Pattern Expr -> RewriteCondition (Maybe Double) Expr
is_const v subst egr =
    isJust (egr^._class (unsafeGetSubst v subst)._data)

is_const_or_distinct_var :: Pattern Expr -> Pattern Expr -> RewriteCondition (Maybe Double) Expr
is_const_or_distinct_var v w subst egr =
    let v' = unsafeGetSubst v subst
        w' = unsafeGetSubst w subst
     in (eClassId (egr^._class v') /= eClassId (egr^._class w'))
        && (isJust (egr^._class v'._data)
            || any ((\case (Sym _) -> True; _ -> False) . unNode) (egr^._class v'._nodes))

rewrites :: [Rewrite (Maybe Double) Expr]
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
    , powP "a" "b" * "a" := powP "a" ("b" + 1)
    , powP "a" 0 := 1 :| is_not_zero "a"
    , powP "a" 1 := "a"
    , "a" * "a" := powP "a" 2
    , powP "a" (fromInteger $ -1) := 1/"a" :| is_not_zero "a"

    , "x"*(1/"x") := 1 :| is_not_zero "x"

    -- In principle, this with distributivity should capture the
    -- binomial theorem, but there is some trickiness.
    , powP "a" "n" := "a" * powP "a" ("n" - 1) :| is_int "n" :| is_positive "n"
    ]

    -- How can the binomial theorem be represented?
    -- Is it really only available for one integer at a time?
    -- ++ [ powP ("a" + "b") (NonVariablePattern . Const $ fromIntegral n) := sum [(fromInteger $ n `_choose` k) * powP "a" (fromInteger k) * powP "b" (fromInteger $ n - k) | k <- [0..n]] | n <- [2..1000]]
    ++

    -- It's a bit unclear to me how to determine that high powers
    -- can be reduced. Ideally something like:
    -- (cos x)^(2n) --> (1-sin^2 x)^n could happen.
    [ powP (cosP "x") 2 := 1 - powP (sinP "x") 2
    , powP (coshP "x") 2 := 1 + powP (sinhP "x") 2
    , powP (cnP "x" "k") 2 := 1 -  powP (snP "x" "k") 2
    , powP (dnP "x" "k") 2 := (1 - powP (snP "x" "k") 2) / powP "k" 2

    , sinP 0 := 0
    , cosP 0 := 1
    , coshP 0 := 1
    , snP 0 "k" := 0
    , cnP 0 "k" := 1
    , dnP 0 "k" := 1 / powP "k" 2

    , sinP (fromInteger (-1) * "x") := fromInteger (-1) * sinP "x"
    , cosP (fromInteger (-1) * "x") := cosP "x"
    , sinhP (fromInteger (-1) * "x") := fromInteger (-1) * sinhP "x"
    , coshP (fromInteger (-1) * "x") := coshP "x"
    , snP (fromInteger (-1) * "x") "k" := fromInteger (-1) * snP "x" "k"
    , cnP (fromInteger (-1) * "x") "k" := cnP "x" "k"
    , dnP (fromInteger (-1) * "x") "k" := dnP "x" "k"

    , snP "x" 0 := sinP "x"
    , snP "x" 1 := tanhP "x"
    , cnP "x" 0 := cosP "x"
    , cnP "x" 1 := powP (sechP "x") 2
    , dnP "x" 0 := 1
    , dnP "x" 1 := powP (sechP "x") 2
    , cosP ("x" + "y") := cosP "x" * cosP "y" - sinP "x" * sinP "y"
    , sinP ("x" + "y") := sinP "x" * cosP "y" + cosP "x" * sinP "y"
    , coshP ("x" + "y") := coshP "x" * coshP "y" + sinhP "x" * sinhP "y"
    , sinhP ("x" + "y") := sinhP "x" * coshP "y" + coshP "x" * sinhP "y"
    , snP ("x" + "y") "k" := (snP "x" "k" * cnP "y" "k" * dnP "y" "k" + snP "y" "k" * cnP "x" "k" * dnP "x" "k") / (1 - powP "k" 2 * powP (snP "x" "k") 2 * powP (snP "y" "k") 2)
    , cnP ("x" + "y") "k" := (cnP "x" "k" * cnP "y" "k" - snP "x" "k" * snP "y" "k" * dnP "x" "k" * dnP "y" "k") / (1 - powP "k" 2 * powP (snP "x" "k") 2 * powP (snP "y" "k") 2)
    , dnP ("x" + "y") "k" := (dnP "x" "k" * dnP "y" "k" - powP "k" 2 * snP "x" "k" * snP "y" "k" * cnP "x" "k" * cnP "y" "k") / (1 - powP "k" 2 * powP (snP "x" "k") 2 * powP (snP "y" "k") 2)

    , diffP "x" "x" := 1 :| is_sym "x"
    , diffP "x" "c" := 0 :| is_sym "x" :| is_const_or_distinct_var "c" "x"

    , diffP "x" ("a" + "b") := diffP "x" "a" + diffP "x" "b"
    , diffP "x" ("a" * "b") := ("a"*diffP "x" "b") + ("b"*diffP "x" "a")

    , diffP "x" (sinP "x") := cosP "x"
    , diffP "x" (cosP "x") := fromInteger (-1) * sinP "x"

    , diffP "x" (sinhP "x") := coshP "x"
    , diffP "x" (coshP "x") := sinhP "x"

    , diffP "x" (snP "x" "k") := cnP "x" "k" * dnP "x" "k"
    , diffP "x" (cnP "x" "k") := fromInteger (-1) * snP "x" "k" * dnP "x" "k"
    , diffP "x" (dnP "x" "k") := fromInteger (-1) * powP "k" 2 * snP "x" "k" * cnP "x" "k"

    , diffP "x" (lnP "x") := 1/"x" :| is_not_zero "x"

    -- diff-power
    , diffP "x" (powP "f" "g") := powP "f" "g" * ((diffP "x" "f" * ("g" / "f")) +
        (diffP "x" "g" * lnP "f")) :| is_not_zero "f" :| is_not_zero "g"

    -- i-one
    , intP 1 "x" := "x"

    , intP (1/"x") "x" := lnP "x"
    -- i power const
    -- This may have trouble if c is -1.
    , intP (powP "x" "c") "x" := (/) (powP "x" ((+) "c" 1)) ((+) "c" 1) :| is_const "c"

    , intP (cosP "x") "x" := sinP "x"
    , intP (sinP "x") "x" := fromInteger (-1)*cosP "x"
    , intP (coshP "x") "x" := sinhP "x"
    , intP (sinhP "x") "x" := coshP "x"

    , intP ("f" + "g") "x" := intP "f" "x" + intP "g" "x"

    , intP ("f" - "g") "x" := intP "f" "x" - intP "g" "x"

    , intP ("a" * "b") "x" := (-) ((*) "a" (intP "b" "x")) (intP ((*) (diffP "x" "a") (intP "b" "x")) "x")

    -- Additional ad-hoc: because of negate representations?
    , "a"-(fromInteger (-1)*"b") := "a"+"b"

    ] where
    n `_choose` k
      | k < 0 || k > n = 0
      | k == 0 || k == n = 1
      | k == 1 || k == n - 1 = n
      | 2 * k > n = n `_choose` (n - k)
      | otherwise = (n - 1) `_choose` (k - 1) * n `div` k

rewrite :: Fix Expr -> Fix Expr
rewrite e = fst $ equalitySaturation e rewrites symCost

symTests :: TestTree
symTests = testGroup "Jacobi"
    [ testCase "(a*2)/2 = a (custom rules)" $
        fst (equalitySaturation @(Maybe Double) (("a"*2)/2) [ ("x"*"y")/"z" := "x"*("y"/"z")
                                                            , "y"/"y" := 1
                                                            , "x"*1 := "x"] symCost) @?= "a"

    , testCase "(a/2)*2 = a (all rules)" $
        rewrite (("a"/2)*2) @?= "a"

    , testCase "(a+a)/2 = a (extra rules)" $
        rewrite (("a"+"a")/2) @?= "a"

    , testCase "x/y (custom rules)" $
        -- without backoff scheduler this will loop forever
        fst (equalitySaturation @(Maybe Double)
                ("x"/"y")

                [ "x"/"y" := "x"*(1/"y")
                , "x"*("y"*"z") := ("x"*"y")*"z"
                ]

                symCost) @?= ("x"/"y")

    , testCase "0+1 = 1 (all rules)" $
        fst (equalitySaturation (0+1) rewrites symCost)   @?= 1

    , testCase "b*(1/b) = 1 (custom rules)" $
        fst (equalitySaturation @(Maybe Double) ("b"*(1/"b")) [ "a"*(1/"a") := 1 ] symCost) @?= 1

    , testCase "1+1=2 (constant folding)" $
        fst (equalitySaturation @(Maybe Double) (1+1) [] symCost) @?= 2

    , testCase "a*(2-1) (1 rule + constant folding)" $
        fst (equalitySaturation @(Maybe Double) ("a" * (2-1)) ["x"*1:="x"] symCost) @?= "a"

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

    -- Trig identities might be a stepping stone to the elliptic.
    , testCase "trig add thm: sin(a+b) = sin a * cos b + cos a * sin b" $
        rewrite (_sin("a" + "b")) @?= _sin "a" * _cos "b" + _cos "a" * _sin "b"

    -- TODO: More elliptic function identities may be worthwhile.
    , testCase "reduce (dn(x,k))^11 in terms of sn(x,k)" $
        fst (equalitySaturation' (defaultBackoffScheduler { banLength = 100 }) (_pow (_dn "x" "k") 11) rewrites depthCost) @?= _pow ((1 - _pow (_sn "x" "k") 2) / _pow "k" 2) 5 * _dn "x" "k" -- this should actually not be equal

    , testCase "reduce (dn(x,k))^1001 in terms of sn(x,k)" $
        rewrite (_pow (_dn "x" "k") 1001) @?= _pow ((1 - _pow (_sn "x" "k") 2) / _pow "k" 2) 500 * _dn "x" "k"

    , testCase "cubic binomial (a+b)^3 = a^3 + 3*a^2*b + 3*a*b^2 + b^3" $
        rewrite (_pow ("a" + "b") 3) @?= _pow "a" 3 + fromInteger 3 * _pow "a" 2 * "b" + fromInteger 3 * "a" * _pow "b" 2 + _pow "b" 3

    -- TODO: Require ability to fine tune parameters
    -- , testCase "diff_power_harder" $
    --     rewrite (_d "x" ((_pow "x" 3) - 7*(_pow "x" 2))) @?= "x"*(3*"x"-14)

    ]

_i, _d, _pow, _sn, _cn, _dn :: Fix Expr -> Fix Expr -> Fix Expr
_i a b = Fix (BinOp Integral a b)
_d a b = Fix (BinOp Diff a b)
_pow a b = Fix (BinOp Pow a b)
_sn x k = Fix $ BinOp Sn x k
_cn x k = Fix $ BinOp Cn x k
_dn x k = Fix $ BinOp Dn x k

_ln, _cos, _sin, _cosh, _sinh :: Fix Expr -> Fix Expr
_ln a = Fix (UnOp Ln a)
_cos a = Fix (UnOp Cos a)
_sin a = Fix (UnOp Sin a)
_cosh a = Fix (UnOp Cosh a)
_sinh a = Fix (UnOp Sinh a)

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

coshP :: Pattern Expr -> Pattern Expr
coshP a = NonVariablePattern (UnOp Cosh a)

sinhP :: Pattern Expr -> Pattern Expr
sinhP a = NonVariablePattern (UnOp Sinh a)

tanhP :: Pattern Expr -> Pattern Expr
tanhP a = NonVariablePattern (Prod [NonVariablePattern $ UnOp Sinh a, NonVariablePattern $ BinOp Pow (NonVariablePattern $ UnOp Cosh a) (NonVariablePattern . Const $ -1)])

sechP :: Pattern Expr -> Pattern Expr
sechP a = NonVariablePattern $ BinOp Pow (NonVariablePattern $ UnOp Cosh a) (NonVariablePattern . Const $ -1)

lnP :: Pattern Expr -> Pattern Expr
lnP a = NonVariablePattern (UnOp Ln a)

snP :: Pattern Expr -> Pattern Expr -> Pattern Expr
snP x k = NonVariablePattern $ BinOp Sn x k

cnP :: Pattern Expr -> Pattern Expr -> Pattern Expr
cnP x k = NonVariablePattern $ BinOp Cn x k

dnP :: Pattern Expr -> Pattern Expr -> Pattern Expr
dnP x k = NonVariablePattern $ BinOp Dn x k
