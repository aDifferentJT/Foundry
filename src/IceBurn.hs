{-# LANGUAGE NoImplicitPrelude, NumericUnderscores, OverloadedStrings #-}
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

import Control.Monad.Except (ExceptT(..), throwError)
import Data.List.Split (chunksOf)

burn :: ByteString -> ExceptT Text IO ()
burn bs =
  withBoard $ \board ->
    boardWithGpio board $ \gpio -> do
      gpioSetReset gpio True
      boardWithSpi board 0 $ \spi -> do
        _ <- spiSetSpeed spi 50_000_000
        spiSetMode spi

        let flash = M25P10Flash . spiIoFunc $ spi
        flashWakeup flash
        flashId <- flashGetId flash
        unless (flashId == "\x20\x20\x11") . throwError
          $ "ID incorrect, this may not be the correct flash, it has ID " ++ (tshow . unpack $ flashId)

        -- Erase
        flashChipErase flash

        -- Write
        mapM_ (\xs -> flashPageProgram flash (maybe 0 fst . headMay $ xs) . pack . map snd $ xs) . chunksOf 256 . zip [0..] . unpack $ bs

        -- Verify
        buf <- flashRead flash 0 (length bs)
        unless (buf == bs) . throwError $ "Image doesn't match"

      gpioSetReset gpio False

