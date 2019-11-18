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
  , forgive
  , throwErrors
  ) where

import ClassyPrelude

import Control.Monad.Except (MonadError, throwError, catchError)

data Errors e a
  = Recovered [e] a
  | Unrecovered (NonNull [e])

concatL :: IsSequence mono => mono -> NonNull mono -> NonNull mono
concatL xs ys = case uncons xs of
  Nothing      -> ys
  Just (x,xs') -> ncons x $ xs' ++ toNullable ys

concatR :: IsSequence mono => NonNull mono -> mono -> NonNull mono
concatR xs ys =
  let (x, xs') = splitFirst xs in
  ncons x $ xs' <> ys

_getErrors :: Errors e a -> [e]
_getErrors (Recovered es _) = es
_getErrors (Unrecovered es) = toNullable es

runErrors :: Errors e a -> Either [e] a
runErrors (Recovered [] x) = Right x
runErrors (Recovered es _) = Left . reverse $ es
runErrors (Unrecovered es) = Left . reverse . toNullable $ es

recover :: a -> Errors e a -> Errors e a
recover _ (Recovered es x) = Recovered es x
recover x (Unrecovered es) = Recovered (toNullable es) x

unrecover :: Errors e a -> Errors e a
unrecover (Recovered [] x)     = Recovered [] x
unrecover (Recovered (e:es) _) = Unrecovered $ ncons e es
unrecover (Unrecovered es)     = Unrecovered es

forgive :: Errors e a -> Errors e a
forgive (Recovered _ x)  = Recovered [] x
forgive (Unrecovered es) = Unrecovered es

instance Functor (Errors e) where
  fmap f (Recovered es x) = Recovered es (f x)
  fmap _ (Unrecovered es) = Unrecovered es

instance Applicative (Errors e) where
  pure = Recovered []
  (Recovered es1 f) <*> (Recovered es2 x) = Recovered (es2 ++ es1) (f x)
  (Recovered es1 _) <*> (Unrecovered es2) = Unrecovered $ concatR es2 es1
  (Unrecovered es1) <*> (Recovered es2 _) = Unrecovered $ concatL es2 es1
  (Unrecovered es1) <*> (Unrecovered es2) = Unrecovered $ es2 <> es1

instance Monad (Errors e) where
  (Recovered es1 x) >>= f = case f x of
    Recovered es2 y -> Recovered (es2 ++ es1) y
    Unrecovered es2 -> Unrecovered $ concatR es2 es1
  (Unrecovered es1) >>= _ = Unrecovered es1

instance MonadError e (Errors e) where
  throwError = Unrecovered . flip ncons []
  catchError (Recovered es x) _ = Recovered es x
  catchError (Unrecovered es) f =
    let (e, es') = splitFirst es in
    join . Recovered es' . f $ e

throwErrors :: a -> [e] -> Errors e a
throwErrors x = maybe (Recovered [] x) Unrecovered . fromNullable

