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

import Control.Monad.Except (ExceptT(..), throwError)
import Data.List.Split (chunksOf)

burn :: ByteString -> ExceptT Text IO ()
burn bs = withBoard $ \board -> do
  boardGetSerial board >>= lift . putStrLn . ("Found iCE40 board serial: " ++)
  boardWithGpio board $ \gpio -> do
    gpioSetReset gpio True
    boardWithSpi board 0 $ \spi -> do
      spiSetSpeed spi 50_000_000
      spiSetMode spi

      let flash = M25P10Flash . spiIoFunc $ spi
      flashWakeup flash
      flashId <- flashGetId flash
      unless (flashId == "\x20\x20\x11") . throwError
        $ "ID incorrect, this may not be the correct flash, it has ID " ++ (tshow . unpack $ flashId)

      lift . putStrLn $ "Erasing flash..."
      flashChipErase flash
      lift . putStrLn $ "Flash erased"

      lift . putStrLn $ "Writing image..."
      mapM (\xs -> flashPageProgram flash (maybe 0 fst . headMay $ xs) . pack . map snd $ xs) . chunksOf 256 . zip [0..] . unpack $ bs
      lift . putStrLn $ "Image written"

      lift . putStrLn $ "Verifying written image..."
      buf <- flashRead flash 0 (length bs)
      unless (buf == bs) . throwError $ "Image doesn't match"
      lift . putStrLn $ "Image verified"

      return ()
    gpioSetReset gpio False

