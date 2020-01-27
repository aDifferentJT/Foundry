module IceBurn
  ( module Octet
  , burn
  ) where

import Prelude

import Ice40Board
import Ice40Board (Octet(..)) as Octet

import Control.Monad.Except.Trans (ExceptT, throwError)
import Data.Array (cons, drop, head, length, take, zip, (..))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n xs =
  if length xs <= n
  then [xs]
  else cons (take n xs) (chunksOf n (drop n xs))

burn :: Array Octet -> ExceptT String Aff Unit
burn bs =
  withBoard $ \board ->
    boardWithGpio board $ \gpio -> do
      gpioSetReset gpio true
      boardWithSpi board (Octet 0) $ \spi -> do
        _ <- spiSetSpeed spi 50_000_000
        spiSetMode spi

        let flash = M25P10Flash <<< spiIoFunc $ spi
        flashWakeup flash
        flashId <- flashGetId flash
        unless (flashId == [Octet 0x20, Octet 0x20, Octet 0x11]) <<< throwError
          $ "ID incorrect, this may not be the correct flash, it has ID " <> (show flashId)

        -- Erase
        flashChipErase flash

        -- Write
        _ <- traverse (\xs -> flashPageProgram flash (maybe 0 fst <<< head $ xs) <<< map snd $ xs) <<< chunksOf 256 <<< zip (0 .. length bs) $ bs

        -- Verify
        buf <- flashRead flash 0 (length bs)
        unless (buf == bs) <<< throwError $ "Image doesn't match"

      gpioSetReset gpio false

