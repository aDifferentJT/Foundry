{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude #-}

module Main (main) where

import ClassyPrelude

import Maps.List

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ mapHeadProperties
  , mapHeadTailProperties
  , mapLastProperties
  , mapInitLastProperties
  ]

mapHeadProperties :: TestTree
mapHeadProperties = testGroup "mapHead"
  [ testProperty "null (mapHead f [])" $ \(Fun _ f) ->
    let _ = f :: Int -> Int in
    null (mapHead f [])
  , testProperty "mapHead f [x] == [f x]" $ \(Fun _ f) x ->
    let _ = f :: Int -> Int in
    let _ = x :: Int in
    mapHead f [x] == [f x]
  , testProperty "mapHead f [x, y] == [f x, y]" $ \(Fun _ f) x y ->
    let _ = f :: Int -> Int in
    let _ = x :: Int in
    let _ = y :: Int in
    mapHead f [x, y] == [f x, y]
  ]

mapHeadTailProperties :: TestTree
mapHeadTailProperties = testGroup "mapHeadTail"
  [ testProperty "null (mapHeadTail f [])" $ \(Fun _ f) (Fun _ g) ->
    let _ = f :: Int -> Int in
    let _ = g :: Int -> Int in
    null (mapHeadTail f g [])
  , testProperty "mapHeadTail f g [x] == [f x]" $ \(Fun _ f) (Fun _ g) x ->
    let _ = f :: Int -> Int in
    let _ = g :: Int -> Int in
    let _ = x :: Int in
    mapHeadTail f g [x] == [f x]
  , testProperty "mapHeadTail f g [x, y] == [f x, g y]" $ \(Fun _ f) (Fun _ g) x y ->
    let _ = f :: Int -> Int in
    let _ = g :: Int -> Int in
    let _ = x :: Int in
    let _ = y :: Int in
    mapHeadTail f g [x, y] == [f x, g y]
  , testProperty "mapHeadTail f g (x:ys) == f x : map g ys" $ \(Fun _ f) (Fun _ g) x ys ->
    let _ = f :: Int -> Int in
    let _ = g :: Int -> Int in
    let _ = x :: Int in
    let _ = ys :: [Int] in
    mapHeadTail f g (x:ys) == f x : map g ys
  ]

mapLastProperties :: TestTree
mapLastProperties = testGroup "mapLast"
  [ testProperty "null (mapLast f [])" $ \(Fun _ f) ->
    let _ = f :: Int -> Int in
    null (mapLast f [])
  , testProperty "mapLast f [x] == [f x]" $ \(Fun _ f) x ->
    let _ = f :: Int -> Int in
    let _ = x :: Int in
    mapLast f [x] == [f x]
  , testProperty "mapLast f [x, y] == [x, f y]" $ \(Fun _ f) x y ->
    let _ = f :: Int -> Int in
    let _ = x :: Int in
    let _ = y :: Int in
    mapLast f [x, y] == [x, f y]
  ]

mapInitLastProperties :: TestTree
mapInitLastProperties = testGroup "mapInitLast"
  [ testProperty "null (mapInitLast f [])" $ \(Fun _ f) (Fun _ g) ->
    let _ = f :: Int -> Int in
    let _ = g :: Int -> Int in
    null (mapInitLast f g [])
  , testProperty "mapInitLast f g [x] == [g x]" $ \(Fun _ f) (Fun _ g) x ->
    let _ = f :: Int -> Int in
    let _ = g :: Int -> Int in
    let _ = x :: Int in
    mapInitLast f g [x] == [g x]
  , testProperty "mapInitLast f g [x, y] == [f x, g y]" $ \(Fun _ f) (Fun _ g) x y ->
    let _ = f :: Int -> Int in
    let _ = g :: Int -> Int in
    let _ = x :: Int in
    let _ = y :: Int in
    mapInitLast f g [x, y] == [f x, g y]
  , testProperty "mapInitLast f g xs == (map fst . mapLast swap . map (f *** g) $ xs)" $ \(Fun _ f) (Fun _ g) xs ->
    let _ = f :: Int -> Int in
    let _ = g :: Int -> Int in
    let _ = xs :: [Int] in
    mapInitLast f g xs == (map fst . mapLast swap . map (f &&& g) $ xs)
  ]

