{-# LANGUAGE NoImplicitPrelude, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

{-|
Module      : Parser.AlexPosn
Description : The data structure representing a position in the input stream
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental

This is a data structure representing a position in the input stream together with an applicative functor for wrapping values with a corresponding section of the input stream for nice error messages
-}
module Parser.AlexPosn
  ( AlexPosn(AlexPosn)
  , Locatable(Locatable, locatableValue, locatablePosns)
  ) where

import ClassyPrelude

import Utils ((****))

import Control.Applicative (liftA2)

-- | `AlexPosn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
-- and column of a token within the file. @start_pos@ gives the position of the
-- start of the file and @eof_pos@ a standard encoding for the end of file.
-- @move_pos@ calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.
data AlexPosn = AlexPosn !Int !Int !Int -- ^ Construct an `AlexPosn' with @start_pos@, @eof_pos@ and @move_pos@
  deriving (Eq, Ord, Show)

-- | A wrapper for values with a corresponding location in the input stream
data Locatable a = Locatable
  { locatableValue :: a                          -- ^ The value
  , locatablePosns :: Maybe (AlexPosn, AlexPosn) -- ^ Possibly the corresponding start and end positions in the input stream
  }
  deriving Show

instance Functor Locatable where
  fmap f Locatable{..} = Locatable{ locatableValue = f locatableValue, .. }

-- | This has the new location spanning the locations of the inputs
instance Applicative Locatable where
  pure x = Locatable x Nothing
  (Locatable f ps1)     <*> (Locatable x Nothing) = Locatable (f x) ps1
  (Locatable f Nothing) <*> (Locatable x ps2)     = Locatable (f x) ps2
  (Locatable f ps1)     <*> (Locatable x ps2)     = Locatable (f x) (liftA2 (min **** max) ps1 ps2)
