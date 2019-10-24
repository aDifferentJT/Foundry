{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, NoImplicitPrelude #-}

{-|
Module      : Parser.Errors
Description : A monad capable of logging multiple errors
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Parser.Errors
  ( Errors
  , runErrors
  , recover
  , unrecover
  , throwErrors
  ) where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError, catchError)

data Errors e a
  = Recovered [e] a
  | Unrecovered [e]

_getErrors :: Errors e a -> [e]
_getErrors (Recovered es _) = es
_getErrors (Unrecovered es) = es

runErrors :: Errors e a -> Either [e] a
runErrors (Recovered [] x) = Right x
runErrors (Recovered es _) = Left . reverse $ es
runErrors (Unrecovered es) = Left . reverse $ es

recover :: a -> Errors e a -> Errors e a
recover _ (Recovered es x) = Recovered es x
recover x (Unrecovered es) = Recovered es x

unrecover :: Errors e a -> Errors e a
unrecover (Recovered [] x) = Recovered [] x
unrecover (Recovered es _) = Unrecovered es
unrecover (Unrecovered es) = Unrecovered es

instance Functor (Errors e) where
  fmap f (Recovered es x) = Recovered es (f x)
  fmap _ (Unrecovered es) = Unrecovered es

instance Applicative (Errors e) where
  pure = Recovered []
  (Recovered es1 f) <*> (Recovered es2 x) = Recovered (es2 ++ es1) (f x)
  f <*> x = Unrecovered (_getErrors x ++ _getErrors f)

instance Monad (Errors e) where
  (Recovered es1 x) >>= f = case f x of
    Recovered es2 y -> Recovered (es2 ++ es1) y
    Unrecovered es2 -> Unrecovered (es2 ++ es1)
  (Unrecovered es1) >>= _ = Unrecovered es1

instance MonadError e (Errors e) where
  throwError = Unrecovered . (:[])
  catchError (Recovered es x)     _ = Recovered es x
  catchError (Unrecovered (e:es)) f = join . Recovered es . f $ e

throwErrors :: a -> [e] -> Errors e a
throwErrors x [] = Recovered [] x
throwErrors _ es = Unrecovered es

