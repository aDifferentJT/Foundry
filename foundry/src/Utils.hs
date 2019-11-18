{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings, RankNTypes, TupleSections, TypeFamilies, UndecidableInstances #-}

{-|
Module      : Utils
Description : Random utility functions
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental

A few random utility functions, if there were enough of these I would consider splitting it into different modules and possibly a separate package.
-}
module Utils
  ( groupWith
  , (****)
  , zipMaybe
  , wrapError
  , flap
  ) where

import ClassyPrelude

import Control.Arrow ((***))
import Control.Monad.Except (MonadError, catchError, throwError)
import qualified Data.Map.Strict as Map

-- | Group the elements by their image under the function
groupWith :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupWith f = Map.toList . groupWith' f
  where groupWith' :: Ord b => (a -> b) -> [a] -> Map.Map b [a]
        groupWith' f' = foldr (\x -> Map.insertWith (++) (f' x) [x]) Map.empty

-- | Combine two binary functions to give a binary function on pairs
(****) :: (a -> b -> c)    -- ^ A function \(f\)
       -> (a' -> b' -> c') -- ^ A function \(g\)
       -> (a, a')          -- ^ Values \((x_1, y_1)\)
       -> (b, b')          -- ^ Values \((x_2, y_2)\)
       -> (c, c')          -- ^ \((f(x_1, x_2), g(x_1, x_2))\)
f **** g = uncurry (***) . (f *** g)

zipMaybe :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipMaybe  []     ys    = map ((Nothing,) . Just) ys
zipMaybe  xs     []    = map ((,Nothing) . Just) xs
zipMaybe (x:xs) (y:ys) = (Just x, Just y) : zipMaybe xs ys

wrapError :: MonadError e m => m () -> m a -> m () -> m a
wrapError pre act post = do
  pre
  res <- catchError (Left <$> act) (return . Right)
  post
  case res of
    Left x  -> return x
    Right e -> throwError e

flap :: Functor f => f (a -> b) -> a -> f b
flap fs x = fmap ($ x) fs

