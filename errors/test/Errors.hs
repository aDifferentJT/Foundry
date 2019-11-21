{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-partial-type-signatures #-}

module Main (main) where

import ClassyPrelude

import Control.Monad.Errors

import Control.Monad.Except (catchError, throwError)

import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.Laws
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ errorsProperties
  ]

instance (Arbitrary mono, MonoFoldable mono) => Arbitrary (NonNull mono) where
  arbitrary = suchThatMap arbitrary fromNullable

instance (Show e, Show a) => Show (Errors e a) where
  show (Recovered es x) = "Recovered " ++ show es ++ " " ++ show x
  show (Unrecovered es) = "Unrecovered " ++ (show . toNullable $ es)

instance (Eq e, Eq a) => Eq (Errors e a) where
  (Recovered es1 x) == (Recovered es2 y) = es1 == es2 && x == y
  (Unrecovered es1) == (Unrecovered es2) = es1 == es2
  _ == _ = False

instance (Arbitrary e, Arbitrary a) => Arbitrary (Errors e a) where
  arbitrary = oneof
    [ Recovered <$> arbitrary <*> arbitrary
    , Unrecovered <$> arbitrary
    ]

errorsProperties :: TestTree
errorsProperties = testGroup "Errors"
  [ functorLaws     (LawType :: LawType (Errors Text))
  , applicativeLaws (LawType :: LawType (Errors Text))
  , monadLaws       (LawType :: LawType (Errors Text))
  , monadErrorProperties
  , throwErrorsProperties
  , runErrorsProperties
  , recoverProperties
  , unrecoverProperties
  , forgiveProperties
  ]

monadErrorProperties :: TestTree
monadErrorProperties = testGroup "MonadError Properties"
  [ testProperty "Catch return" $ \(Fun _ f) x ->
    let _ = f :: Text -> Errors Text Int in
    let _ = x :: Int in
    catchError (return x) f == return x
  , testProperty "Catch throw" $ \(Fun _ f) e ->
    let _ = f :: Text -> Errors Text Int in
    let _ = e :: Text in
    catchError (throwError e) f == f e
  ]

throwErrorsProperties :: TestTree
throwErrorsProperties = testGroup "throwErrors"
  [ testProperty "throwErrors x [] == return x" $ \x ->
    throwErrors x [] == (return x :: Errors Text Int)
  , testProperty "throwErrors () es == mapM_ throwError es" $ \es ->
    throwErrors () es == (mapM_ throwError es :: Errors Text ())
  ]

runErrorsProperties :: TestTree
runErrorsProperties = testGroup "runErrors"
  [ testProperty "runErrors . return == Right" $ \x ->
    (runErrors . return) x == (Right x :: Either [Text] Int)
  , testProperty "runErrors . throwError == Left . (:[])" $ \e ->
    (runErrors . throwError) e == (Left [e] :: Either [Text] Int)
  , testProperty "runErrors . throwErrors x == Left" $ \x es ->
    let _ = x :: Int in
    let es' = getNonEmpty es :: [Text] in
    (runErrors . throwErrors x) es' == Left es'
  , testProperty "runErrors (throwErrors x []) == Right x" $ \x ->
    (runErrors . throwErrors x) [] == (Right x :: Either [Text] Int)
  , testProperty "runErrors . recover x . throwErrors y == Left" $ \x y es ->
    let _ = x :: Int in
    let _ = y :: Int in
    let es' = getNonEmpty es :: [Text] in
    (runErrors . recover x . throwErrors y) es' == Left es'
  ]

recoverProperties :: TestTree
recoverProperties = testGroup "recover"
  [ testProperty "recover x . return == return" $ \x y ->
    (recover x . return) y == (return y :: Errors Text Int)
  , testProperty "(recover x (throwError e) >>= f) == recover y (throwError e) >> f x" $ \e (Fun _ f) x y ->
    let _ = e :: Text in
    let _ = f :: Int -> Errors Text Int in
    let _ = x :: Int in
    let _ = y :: Int in
    (recover x (throwError e) >>= f) == (recover y (throwError e) >> f x)
  , testProperty "(recover x (throwErrors x es) >>= f) == recover y (throwErrors y es) >> f x" $ \x y es (Fun _ f) ->
    let _ = x :: Int in
    let _ = y :: Int in
    let _ = es :: [Text] in
    let _ = f :: Int -> Errors Text Int in
    (recover x (throwErrors x es) >>= f) == (recover y (throwErrors y es) >> f x)
  ]

unrecoverProperties :: TestTree
unrecoverProperties = testGroup "unrecover"
  [ testProperty "unrecover . return == return" $ \x ->
    (unrecover . return) x == (return x :: Errors Text Int)
  , testProperty "unrecover . throwError == throwError" $ \e ->
    (unrecover . throwError) e == (throwError e :: Errors Text Int)
  , testProperty "unrecover . throwErrors y == throwErrors y" $ \x es ->
    let _ = x :: Int in
    let es' = getNonEmpty es :: [Text] in
    (unrecover . throwErrors x) es' == throwErrors x es'
  , testProperty "unrecover . recover x . throwError == throwError" $ \x e ->
    let _ = x :: Int in
    let _ = e :: Text in
    (unrecover . recover x . throwError) e == throwError e
  , testProperty "unrecover . recover x . throwErrors y == throwErrors y" $ \x y es ->
    let _ = x :: Int in
    let _ = y :: Int in
    let es' = getNonEmpty es :: [Text] in
    (unrecover . recover x . throwErrors y) es' == throwErrors y es'
  ]

forgiveProperties :: TestTree
forgiveProperties = testGroup "forgive"
  [ testProperty "forgive (throwError e) == throwError e" $ \e ->
    forgive (throwError e) == (throwError e :: Errors Text Int)
  , testProperty "forgive (throwErrors x es) == throwErrors x es" $ \x es ->
    let _ = x :: Int in
    let _ = es :: [Text] in
    forgive (throwErrors x es) == throwErrors x es
  , testProperty "(forgive . recover x . throwError $ e) == return x" $ \x e ->
    let _ = x :: Int in
    let _ = e :: Text in
    (forgive . recover x . throwError $ e) == return x
  , testProperty "(forgive . recover x . throwErrors y $ es) == return x" $ \x y es ->
    let _ = x :: Int in
    let _ = y :: Int in
    let es' = getNonEmpty es :: [Text] in
    (forgive . recover x . throwErrors y $ es') == return x
  ]

