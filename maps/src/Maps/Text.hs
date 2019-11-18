{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-|
Module      : Maps.Text
Description : Maps over a text
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Maps.Text
  ( mapHead
  , textHeadToUpper
  ) where

import ClassyPrelude

import qualified Data.Char as Char
import qualified Data.Text as Text

mapHead :: (Char -> Char) -> Text -> Text
mapHead f = maybe "" (uncurry Text.cons . first f) . Text.uncons

textHeadToUpper :: Text -> Text
textHeadToUpper = mapHead Char.toUpper

