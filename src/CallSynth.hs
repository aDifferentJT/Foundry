{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, TupleSections #-}

{-|
Module      : CallSynth
Description : Call out to the sythesizer
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module CallSynth
  ( callSynth
  ) where

import ClassyPrelude

import Utils (readProcess)

import Paths_Foundry

import Data.Text.IO (hPutStrLn)
import System.FilePath ((-<.>), takeBaseName)
import System.IO (Handle)
import System.Process (callProcess)

writeVerilog :: Handle -> Text -> IO ()
writeVerilog h verilog = hPutStrLn h verilog >> getDataFileName "verilog/wrapper.v" >>= readFile >>= hPut h

makeBlif :: FilePath -> FilePath -> IO ()
makeBlif blifFn verilogFn = do
  putStrLn "Making blif..."
  callProcess "yosys" ["-p", "synth_ice40 -top top -blif " ++ blifFn, verilogFn]
  putStrLn "blif made"

makeAsc :: ByteString -> IO ByteString
makeAsc blif = do
  putStrLn "Making asc..."
  pinDefFn <- getDataFileName "verilog/pinDef.pcf"
  asc <- readProcess "arachne-pnr" ["-d", "1k", "-p", pinDefFn, "-P", "vq100"] blif
  putStrLn "asc made"
  return asc

makeBin :: ByteString -> IO ByteString
makeBin asc = do
  putStrLn "Making bin..."
  bin <- readProcess "icepack" [] asc
  putStrLn "bin made"
  return bin

callSynth :: Text -> IO ByteString
callSynth verilog =
  withSystemTempFile ("foundry.v") $ \verilogFn verilogH ->
    withSystemTempFile ("foundry.blif") $ \blifFn blifH -> do
      writeVerilog verilogH verilog
      hFlush verilogH
      makeBlif blifFn verilogFn
      hGetContents blifH >>= makeAsc >>= makeBin

