{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Parser.AlexPosn
  ( AlexPosn(AlexPosn)
  , Locatable(Locatable, locatableValue, locatablePosns)
  ) where

import Utils ((****))

import Control.Applicative (liftA2)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Except (MonadError(throwError, catchError))
import Control.Monad.Signatures (Catch)

-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPosn !Int !Int !Int
  deriving (Eq, Ord, Show)

data Locatable a = Locatable
  { locatableValue :: a
  , locatablePosns :: Maybe (AlexPosn, AlexPosn)
  }
  deriving Show

instance Functor Locatable where
  fmap f Locatable{..} = Locatable{ locatableValue = f locatableValue,.. }

instance Applicative Locatable where
  pure x = Locatable x Nothing
  (Locatable f ps1) <*> (Locatable x Nothing) = Locatable (f x) ps1
  (Locatable f Nothing) <*> (Locatable x ps2) = Locatable (f x) ps2
  (Locatable f ps1) <*> (Locatable x ps2)     = Locatable (f x) (liftA2 (min **** max) ps1 ps2)

{-
instance Monad Locatable where
  (Locatable x ps1) >>= f = let Locatable y ps2 = f x in Locatable y (liftA2 (min **** max) ps1 ps2)
  -}
