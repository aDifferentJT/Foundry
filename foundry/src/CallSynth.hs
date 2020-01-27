{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-|
Module      : CallSynth
Description : Call out to the sythesizer
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module CallSynth
  ( burn
  ) where

import ClassyPrelude

import Paths_foundry

import qualified IceBurn (burn)

import Control.Concurrent (forkIO)
import Control.Monad.Except (ExceptT)
import Data.Text.IO (hPutStrLn)
import System.IO (Handle, openFile)
import System.Process
  ( ProcessHandle
  , StdStream(UseHandle)
  , createPipe
  , createProcess
  , proc
  , std_in
  , std_out
  , std_err
  , waitForProcess
  )

writeVerilog :: Handle -> Text -> IO ()
writeVerilog h verilog = hPutStrLn h verilog >> getDataFileName "verilog/wrapper.v" >>= readFile >>= hPut h

makeBlif :: FilePath -> FilePath -> IO ()
makeBlif blifFn verilogFn = do
  includeDir <- getDataFileName "verilog/"
  void . withFile "/dev/null" WriteMode $ \nullH -> do
    (_, _, _, ph) <- createProcess $
      ( proc "yosys"
        [ "-p"
        , "read_verilog -I" ++ includeDir ++ " " ++ verilogFn ++ " ; synth_ice40 -top top -blif " ++ blifFn
        ]
      )
      { std_out = UseHandle nullH
      }
    waitForProcess ph

makeAsc :: Handle -> Handle -> IO ProcessHandle
makeAsc blifH ascH = do
  pinDefFn <- getDataFileName "verilog/pinDef.pcf"
  nullH <- openFile "/dev/null" WriteMode
  (_, _, _, ph) <- createProcess $
    ( proc "arachne-pnr" ["-d", "1k", "-p", pinDefFn, "-P", "vq100"]
    )
    { std_in  = UseHandle blifH
    , std_out = UseHandle ascH
    , std_err = UseHandle nullH
    }
  _ <- forkIO $ waitForProcess ph >> hClose nullH
  return ph

makeBin :: Handle -> Handle -> IO ProcessHandle
makeBin ascH binH = do
  (_, _, _, ph) <- createProcess $
    ( proc "icepack" []
    )
    { std_in  = UseHandle ascH
    , std_out = UseHandle binH
    }
  return ph

callSynth :: Text -> IO ByteString
callSynth verilog =
  withSystemTempFile "foundry.v" $ \verilogFn verilogH ->
    withSystemTempFile "foundry.blif" $ \blifFn blifH -> do
      writeVerilog verilogH verilog
      hFlush verilogH
      makeBlif blifFn verilogFn
      (ascIn, ascOut) <- createPipe
      (binIn, binOut) <- createPipe
      ascP <- makeAsc blifH ascOut
      binP <- makeBin ascIn binOut
      bin <- hGetContents binIn
      _ <- waitForProcess ascP
      _ <- waitForProcess binP
      return bin

burn :: Text -> ExceptT Text IO ()
burn verilog = (lift . callSynth $ verilog) >>= IceBurn.burn

