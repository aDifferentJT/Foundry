{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude #-}

module Main (main) where

import ClassyPrelude

import Data.List.Group

import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ groupWithProperties
  ]

groupWithProperties :: TestTree
groupWithProperties = testGroup "groupWith"
  [ testProperty "NonEmpty ys => Set.fromList (groupWith (const x) ys) == Set.fromList [(x, ys)]" $ \x ys ->
    let _ = x :: Int in
    let ys' = getNonEmpty ys :: [Int] in
    Set.fromList (groupWith (const x) ys') == Set.fromList [(x, ys')]
  , testProperty "Set.fromList . map (first Just) . groupWith id == Set.fromList . map (headMay &&& id) . group . sort" $ \xs ->
    let _ = xs :: [Int] in
    (Set.fromList . map (first Just) . groupWith id $ xs) == (Set.fromList . map (headMay &&& id) . group . sort $ xs)
  ]

