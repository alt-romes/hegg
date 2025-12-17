{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-|
Test module for singleton infrastructure.
-}
module Singleton where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Equality.Utils.Singleton
import Data.Equality.Utils.HList
import Data.Functor.Identity

-- | Test type universe
data Ty = TyInt | TyBool | TyFunc Ty Ty
    deriving (Eq, Show)

-- | Singleton instances for Ty
data instance Sing (t :: Ty) where
    STyInt  :: Sing 'TyInt
    STyBool :: Sing 'TyBool
    STyFunc :: Sing a -> Sing b -> Sing ('TyFunc a b)

-- Show instance for Sing Ty
instance Show (Sing (t :: Ty)) where
    show STyInt = "STyInt"
    show STyBool = "STyBool"
    show (STyFunc a b) = "STyFunc (" ++ show a ++ ") (" ++ show b ++ ")"


-- | SingI instances for Ty
instance SingI 'TyInt where
    sing = STyInt

instance SingI 'TyBool where
    sing = STyBool

instance (SingI a, SingI b) => SingI ('TyFunc a b) where
    sing = STyFunc sing sing

-- | SDecide instance for Ty
instance SDecide Ty where
    decEq STyInt STyInt = Just Refl
    decEq STyBool STyBool = Just Refl
    decEq (STyFunc a1 b1) (STyFunc a2 b2) = do
        Refl <- decEq a1 a2
        Refl <- decEq b1 b2
        Just Refl
    decEq _ _ = Nothing

-- | SOrd instance for Ty (for testing Ord on SomeType)
instance SOrd Ty where
    sCompare STyInt STyInt = SEQ
    sCompare STyInt _ = SLT
    sCompare STyBool STyInt = SGT
    sCompare STyBool STyBool = SEQ
    sCompare STyBool (STyFunc _ _) = SLT
    sCompare (STyFunc _ _) STyInt = SGT
    sCompare (STyFunc _ _) STyBool = SGT
    sCompare (STyFunc a1 b1) (STyFunc a2 b2) = case sCompare a1 a2 of
        SLT -> SLT
        SGT -> SGT
        SEQ -> sCompare b1 b2

-- | Test suite for singleton infrastructure
singletonTests :: TestTree
singletonTests = testGroup "Singleton Infrastructure"
    [ testGroup "SingI"
        [ testCase "sing @TyInt returns STyInt" $
            show (sing :: Sing 'TyInt) @?= "STyInt"
        , testCase "sing @TyBool returns STyBool" $
            show (sing :: Sing 'TyBool) @?= "STyBool"
        , testCase "sing @(TyFunc TyInt TyBool) returns correct structure" $
            show (sing :: Sing ('TyFunc 'TyInt 'TyBool)) @?= "STyFunc (STyInt) (STyBool)"
        ]
    , testGroup "SDecide"
        [ testCase "decEq STyInt STyInt = Just Refl" $
            case decEq STyInt STyInt of
                Just Refl -> return ()
                Nothing -> assertFailure "Expected Just Refl"
        , testCase "decEq STyBool STyBool = Just Refl" $
            case decEq STyBool STyBool of
                Just Refl -> return ()
                Nothing -> assertFailure "Expected Just Refl"
        , testCase "decEq STyInt STyBool = Nothing" $
            decEq STyInt STyBool @?= Nothing
        , testCase "decEq STyBool STyInt = Nothing" $
            decEq STyBool STyInt @?= Nothing
        , testCase "decEq on equal function types = Just Refl" $
            case decEq (STyFunc STyInt STyBool) (STyFunc STyInt STyBool) of
                Just Refl -> return ()
                Nothing -> assertFailure "Expected Just Refl"
        , testCase "decEq on different function types = Nothing" $
            decEq (STyFunc STyInt STyBool) (STyFunc STyBool STyInt) @?= Nothing
        ]
    , testGroup "SomeType"
        [ testCase "SomeType can hide type information" $ do
            let intType = SomeType STyInt
                boolType = SomeType STyBool
            -- They're both SomeType Ty
            withSomeType intType (\s -> show s @?= "STyInt")
            withSomeType boolType (\s -> show s @?= "STyBool")
        , testCase "fromSomeType recovers matching type" $ do
            let intType = SomeType STyInt
            case fromSomeType STyInt intType of
                Just s -> show s @?= "STyInt"
                Nothing -> assertFailure "Expected Just"
        , testCase "fromSomeType returns Nothing for non-matching type" $ do
            let intType = SomeType STyInt
            case fromSomeType STyBool intType of
                Nothing -> return ()
                Just _ -> assertFailure "Expected Nothing"
        , testCase "SomeType Eq: same types are equal" $
            SomeType STyInt == SomeType STyInt @?= True
        , testCase "SomeType Eq: different types are not equal" $
            SomeType STyInt == SomeType STyBool @?= False
        , testCase "SomeType Ord: ordering works" $ do
            -- TyInt < TyBool < TyFunc
            (SomeType STyInt < SomeType STyBool) @?= True
            (SomeType STyBool < SomeType (STyFunc STyInt STyBool)) @?= True
        ]
    , testGroup "SingList"
        [ testCase "singList @'[] returns SNil" $ do
            let sl = singList :: SingList ('[] :: [Ty])
            case sl of
                SNil -> return ()
        , testCase "singList @'[TyInt] returns SCons STyInt SNil" $ do
            let sl = singList :: SingList '[ 'TyInt ]
            case sl of
                SCons STyInt SNil -> return ()
        , testCase "singList @'[TyInt, TyBool] has correct structure" $ do
            let sl = singList :: SingList '[ 'TyInt, 'TyBool ]
            case sl of
                SCons STyInt (SCons STyBool SNil) -> return ()
        , testCase "sListLength SNil = 0" $
            sListLength SNil @?= 0
        , testCase "sListLength single element = 1" $
            sListLength (SCons STyInt SNil) @?= 1
        , testCase "sListLength two elements = 2" $
            sListLength (SCons STyInt (SCons STyBool SNil)) @?= 2
        ]
    , testGroup "SingList equality"
        [ testCase "decEqList on equal lists = Just Refl" $ do
            let sl1 = SCons STyInt (SCons STyBool SNil)
                sl2 = SCons STyInt (SCons STyBool SNil)
            case decEqList sl1 sl2 of
                Just Refl -> return ()
                Nothing -> assertFailure "Expected Just Refl"
        , testCase "decEqList on different lists = Nothing" $ do
            let sl1 = SCons STyInt SNil
                sl2 = SCons STyBool SNil
            decEqList sl1 sl2 @?= Nothing
        , testCase "decEqList on different lengths = Nothing" $ do
            let sl1 = SCons STyInt SNil
                sl2 = SCons STyInt (SCons STyBool SNil)
            decEqList sl1 sl2 @?= Nothing
        ]
    , testGroup "Type-safe coercion with Refl"
        [ testCase "Refl proof enables type-safe operations" $ do
            -- This test demonstrates that Refl can be used for type-safe operations
            let intSing1 = STyInt
                intSing2 = STyInt
            case decEq intSing1 intSing2 of
                Just Refl -> do
                    -- Here, GHC knows the types are equal
                    let _sameSing :: Sing 'TyInt
                        _sameSing = intSing2
                    return ()
                Nothing -> assertFailure "Expected Refl proof"
        ]
    , testGroup "HList"
        [ testCase "HNil has length 0" $
            hLength (HNil :: HList Identity '[]) @?= 0
        , testCase "single element HList has length 1" $
            hLength (Identity (42 :: Int) `HCons` HNil) @?= 1
        , testCase "three element HList has length 3" $ do
            let hlist = Identity (42 :: Int) `HCons`
                        Identity True `HCons`
                        Identity "hello" `HCons`
                        HNil
            hLength hlist @?= 3
        , testCase "hLookup Here retrieves first element" $ do
            let hlist = Identity (42 :: Int) `HCons`
                        Identity True `HCons`
                        HNil
            runIdentity (hLookup Here hlist) @?= 42
        , testCase "hLookup There Here retrieves second element" $ do
            let hlist = Identity (42 :: Int) `HCons`
                        Identity True `HCons`
                        HNil
            runIdentity (hLookup (There Here) hlist) @?= True
        , testCase "hMap transforms all elements" $ do
            -- hMap applies the same transformation to each element
            let hlist = Identity (1 :: Int) `HCons`
                        Identity True `HCons`
                        HNil
            -- Wrap in Maybe (works for all types)
            let wrapped = hMap (\(Identity x) -> Just x) hlist
            hLookup Here wrapped @?= Just (1 :: Int)
            hLookup (There Here) wrapped @?= Just True
        , testCase "hFoldr counts elements correctly" $ do
            let hlist = Identity (1 :: Int) `HCons`
                        Identity True `HCons`
                        Identity "hello" `HCons`
                        HNil
            -- Count elements (ignores the actual value)
            let count = hFoldr (\_ acc -> acc + 1) 0 hlist
            count @?= (3 :: Int)
        , testCase "HList Eq: equal lists" $ do
            let hlist1 = Identity (42 :: Int) `HCons` Identity True `HCons` HNil
                hlist2 = Identity (42 :: Int) `HCons` Identity True `HCons` HNil
            (hlist1 == hlist2) @?= True
        , testCase "HList Eq: unequal lists" $ do
            let hlist1 = Identity (42 :: Int) `HCons` Identity True `HCons` HNil
                hlist2 = Identity (43 :: Int) `HCons` Identity True `HCons` HNil
            (hlist1 == hlist2) @?= False
        , testCase "HList Ord works" $ do
            let hlist1 = Identity (1 :: Int) `HCons` HNil
                hlist2 = Identity (2 :: Int) `HCons` HNil
            (hlist1 < hlist2) @?= True
        ]
    ]
