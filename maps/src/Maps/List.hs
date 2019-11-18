{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings, RankNTypes, TupleSections, TypeFamilies, UndecidableInstances #-}

{-|
Module      : Maps.List
Description : Maps over a list
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Maps.List
  ( mapHead
  , mapHeadTail
  , mapLast
  , mapInitLast
  ) where

import ClassyPrelude

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

