{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import ClassyPrelude

import qualified Maps.List
import Maps.Text

import qualified Data.Char as Char
import qualified Data.Text as Text
import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ mapHeadProperties
  , textHeadToUpperProperties
  ]

mapHeadProperties :: TestTree
mapHeadProperties = testGroup "mapHead"
  [ testProperty "mapHead f == pack . Maps.List.mapHead f . unpack" $ \(Fun _ f) x ->
    let _ = f :: Char -> Char in
    let _ = x :: Text in
    mapHead f x == (pack . Maps.List.mapHead f . unpack $ x)
  ]

textHeadToUpperProperties :: TestTree
textHeadToUpperProperties = testGroup "textHeadToUpper"
  [ testProperty "null (textHeadToUpper \"\")" (null (textHeadToUpper ""))
  , testProperty "textHeadToUpper (Text.singleton c) == Text.singleton (Char.toUpper c)" $ \c ->
    textHeadToUpper (Text.singleton c) == Text.singleton (Char.toUpper c)
  ]

