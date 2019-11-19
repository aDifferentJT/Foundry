{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.List.Group
Description : Group a list
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Data.List.Group
  ( groupWith
  ) where

import ClassyPrelude

import qualified Data.Map.Strict as Map

-- | Group the elements by their image under the function
groupWith :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupWith f = Map.toList . foldr (\x -> Map.insertWith (++) (f x) [x]) Map.empty

