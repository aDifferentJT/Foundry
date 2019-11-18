{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Maps.Either
Description : Maps over an either
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Maps.Either
  ( mapLeft
  ) where

import ClassyPrelude

-- | Map the left side of an Either
mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f = either (Left . f) Right

