{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main) where

import ClassyPrelude

import Maps.Either

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "mapLeft f . Right == Right" $ \(Fun _ f) x ->
    let _ = f :: Int -> Int in
    let _ = x :: Int in
    (mapLeft f . Right) x == Right x
  , testProperty "mapLeft f . Left == Left . f" $ \(Fun _ f) x ->
    let _ = f :: Int -> Int in
    let _ = x :: Int in
    (mapLeft f . Left) x == ((Left . f) x :: Either _ ())
  ]

