{-# LANGUAGE FlexibleInstances, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import ClassyPrelude

import Data.NonNull.Append

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ appendLProperties
  , appendRProperties
  ]

instance (Arbitrary mono, MonoFoldable mono) => Arbitrary (NonNull mono) where
  arbitrary = suchThatMap arbitrary fromNullable

appendLProperties :: TestTree
appendLProperties = testGroup "appendL"
  [ testProperty "toNullable (appendL xs ys) == xs ++ toNullable ys" $ \xs ys ->
    let _ = xs :: [Int] in
    let _ = ys :: NonNull [Int] in
    toNullable (appendL xs ys) == xs ++ toNullable ys
  ]

appendRProperties :: TestTree
appendRProperties = testGroup "appendR"
  [ testProperty "toNullable (appendR xs ys) == toNullable xs ++ ys" $ \xs ys ->
    let _ = xs :: NonNull [Int] in
    let _ = ys :: [Int] in
    toNullable (appendR xs ys) == toNullable xs ++ ys
  ]

