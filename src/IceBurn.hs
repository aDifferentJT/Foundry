{-# LANGUAGE LambdaCase, NoImplicitPrelude, NumericUnderscores, OverloadedStrings, RecordWildCards #-}
{-|
Module      : IceBurn
Description : Burn the data to the iCE40
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module IceBurn
  ( burn
  ) where

import ClassyPrelude

import IceBurn.Ice40Board

import Control.Monad.Except (ExceptT(..))

burn :: ExceptT Text IO ()
burn = withBoard $ \board -> do
  boardGetSerial board >>= lift . putStrLn . ("Found iCE40 board serial: " ++)
  boardWithGpio board $ \gpio -> do
    gpioSetReset gpio True
    boardWithSpi board 0 $ \spi -> do
      spiSetSpeed spi 50_000_000
      spiSetMode spi
      let flash = M25P10Flash . spiIoFunc $ spi
      flashWakeup flash
      flashGetId flash
      flashChipErase flash
      return ()

