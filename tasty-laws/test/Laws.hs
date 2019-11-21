{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import ClassyPrelude

import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.Laws

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ maybeProperties
  , eitherProperties
  ]

maybeProperties :: TestTree
maybeProperties = testGroup "Maybe"
  [ functorLaws     (LawType :: LawType Maybe)
  , applicativeLaws (LawType :: LawType Maybe)
  , monadLaws       (LawType :: LawType Maybe)
  ]

eitherProperties :: TestTree
eitherProperties = testGroup "Either"
  [ functorLaws     (LawType :: LawType (Either Text))
  , applicativeLaws (LawType :: LawType (Either Text))
  , monadLaws       (LawType :: LawType (Either Text))
  ]

