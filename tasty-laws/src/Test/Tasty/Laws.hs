{-# LANGUAGE NoImplicitPrelude, PolyKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Monad law, right identity" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use >=>" #-}

{-|
Module      : Test.Tasty.Laws
Description : Test the functor, applicative and monad laws
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Test.Tasty.Laws
  ( LawType(LawType)
  , functorLaws
  , applicativeLaws
  , monadLaws
  ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.QuickCheck

data LawType a = LawType

functorLaws
  :: forall a . 
  ( Arbitrary (a Int)
  , Eq (a Int)
  , Functor a
  , Show (a Int)
  )
  => LawType a
  -> TestTree
functorLaws _ = testGroup "Functor Laws"
  [ testProperty "Preserve identity" $ \x ->
    let _ = x :: a Int in
    fmap id x == x
  , testProperty "Preserve composition" $ \(Fun _ f) (Fun _ g) x ->
    let _ = f :: Int -> Int in
    let _ = g :: Int -> Int in
    let _ = x :: a Int in
    fmap (f . g) x == (fmap f . fmap g) x
  ]

applicativeLaws
  :: forall a . 
  ( Applicative a
  , Arbitrary (a Int)
  , Arbitrary (a (Fun Int Int))
  , Eq (a Int)
  , Show (a Int)
  , Show (a (Fun Int Int))
  )
  => LawType a
  -> TestTree
applicativeLaws _ = testGroup "Applicative Functor Laws"
  [ testProperty "Identity" $ \v ->
    let _ = v :: a Int in
    (pure id <*> v) == v
  , testProperty "Homomorphism" $ \(Fun _ f) x ->
    let _ = f :: Int -> Int in
    let _ = x :: Int in
    (pure f <*> pure x) == (pure (f x) :: a Int)
  , testProperty "Interchange" $ \u y ->
    let u' = applyFun <$> u :: a (Int -> Int) in
    let _ = y :: Int in
    (u' <*> pure y) == (pure ($ y) <*> u')
  , testProperty "Composition" $ \u v w ->
    let u' = applyFun <$> u :: a (Int -> Int) in
    let v' = applyFun <$> v :: a (Int -> Int) in
    let _ = w :: a Int in
    (pure (.) <*> u' <*> v' <*> w) == (u' <*> (v' <*> w))
  ]

monadLaws
  :: forall a . 
  ( Arbitrary (a Int)
  , Eq (a Int)
  , Monad a
  , Show (a Int)
  )
  => LawType a
  -> TestTree
monadLaws _ = testGroup "Monad Laws"
  [ testProperty "Left identity" $ \x (Fun _ f) ->
    let _ = x :: Int in
    let _ = f :: Int -> a Int in
    (return x >>= f) == f x
  , testProperty "Right identity" $ \m ->
    let _ = m :: a Int in
    (m >>= return) == m
  , testProperty "Associativity" $ \m (Fun _ f) (Fun _ g) ->
    let _ = m :: a Int in
    let _ = f :: Int -> a Int in
    let _ = g :: Int -> a Int in
    ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
  ]

