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
  ( mapLeft
  , mapHead
  , mapHeadTail
  , mapLast
  , mapInitLast
  , isPlural
  , groupWith
  , selectLargestBy
  , intersectionWithKey3
  , (****)
  , joinTailsToHeads
  , zipMaybe
  , wrapError
  , flap
  , textHeadToUpper
  ) where

import ClassyPrelude

import Control.Arrow ((***))
import Control.Monad.Except (MonadError, catchError, throwError)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

-- | Map the left side of an Either
mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f = either (Left . f) Right

-- | Map only the head of the list
mapHead :: (a -> a) -> [a] -> [a]
mapHead _  []    = []
mapHead f (x:xs) = f x : xs

-- | Map seperately the head and tail of the list
mapHeadTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapHeadTail _ _  []    = []
mapHeadTail f g (x:xs) = f x : map g xs

-- | Map only the last element of the list
mapLast :: (a -> a) -> [a] -> [a]
mapLast _  []    = []
mapLast f  [x]   = [f x]
mapLast f (x:xs) = x : mapLast f xs

-- | Map all the elemnts of this list one way bar the last which is mapped a different way
mapInitLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapInitLast _ _ []     = []
mapInitLast _ g [x]    = [g x]
mapInitLast f g (x:xs) = f x : mapInitLast f g xs

-- | Check if a list is plural (more than 1 element)
isPlural :: [a] -> Bool
isPlural []      = False
isPlural [_]     = False
isPlural (_:_:_) = True

-- | Group the elements by their image under the function
groupWith :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupWith f = Map.toList . groupWith' f
  where groupWith' :: Ord b => (a -> b) -> [a] -> Map.Map b [a]
        groupWith' f' = foldr (\x -> Map.insertWith (++) (f' x) [x]) Map.empty

-- | Get the largest element by some metric and the rest
selectLargestBy :: Ord b => (a -> b) -> [a] -> (Maybe a, [a])
selectLargestBy f = first (fst <$>) . foldr g (Nothing, [])
  where g x (Nothing, ys) = (Just (x, f x), ys)
        g x (Just (y, fy), ys)
          | fx <= fy  = (Just (y, fy), x:ys)
          | otherwise = (Just (x, fx), y:ys)
          where fx = f x

intersectionWithKey3 :: Ord k => (k -> a -> b -> c -> d) -> Map k a -> Map k b -> Map k c -> Map k d
intersectionWithKey3 f m1 = Map.intersectionWithKey (uncurry . f) . Map.intersectionWith (,) m1

-- | Combine two binary functions to give a binary function on pairs
(****) :: (a -> b -> c)    -- ^ A function \(f\)
       -> (a' -> b' -> c') -- ^ A function \(g\)
       -> (a, a')          -- ^ Values \((x_1, y_1)\)
       -> (b, b')          -- ^ Values \((x_2, y_2)\)
       -> (c, c')          -- ^ \((f(x_1, x_2), g(x_1, x_2))\)
f **** g = uncurry (***) . (f *** g)

joinTailsToHeads :: Monoid m => m -> [[m]] -> [m]
joinTailsToHeads _ []         = []
joinTailsToHeads _ [x]        = x
joinTailsToHeads y (x1:x2:xs) =
  mapLast ((++ (fromMaybe mempty . headMay $ x2)) . (++ y)) x1
  ++ joinTailsToHeads y ((fromMaybe mempty . tailMay $ x2) : xs)

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

textHeadToUpper :: Text -> Text
textHeadToUpper = maybe "" (uncurry Text.cons . first Char.toUpper) . Text.uncons

