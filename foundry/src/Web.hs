{-# LANGUAGE MultiWayIf, NoImplicitPrelude, OverloadedStrings, TupleSections #-}

{-|
Module      : GenSimulator
Description : Generate a simulator
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Web
  ( hostSimulator
  , hostSimulatorUpdate
  ) where

import ClassyPrelude

import CallSynth (callSynth)
import GenSimulator (genSimulatorBS)
import GenVerilog (genVerilog)

import Control.Concurrent (forkIO)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, throwError)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import qualified Data.Map as Map
import Data.Word ()
import Data.Word.Encode (Endianness(Little), encodeWord32)
import IceBurn (burn)
import Language.Foundry.Parser
import Language.Foundry.Proc
import Paths_foundry
import Snap.Core (Snap)
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap

snapConfig :: Int -> Snap.Config Snap a
snapConfig port =
  Snap.setAccessLog Snap.ConfigNoLog
  . Snap.setErrorLog Snap.ConfigNoLog
  . Snap.setPort port
  $ mempty

getQueryParam :: ByteString -> ExceptT Text Snap ByteString
getQueryParam name = ExceptT $ maybe (Left $ "Could not get \"" ++ decodeUtf8 name ++ "\" from request") Right <$> Snap.getQueryParam name

synth :: FilePath -> ExceptT Text Snap ByteString
synth defFn = getQueryParam "type" >>= bin
  where bin :: ByteString -> ExceptT Text Snap ByteString
        bin "bin" = ByteString.toStrict <$> Snap.readRequestBody (1024 ^ (3 :: Int))
        bin t     = verilog t >>= liftIO . callSynth
        verilog :: ByteString -> ExceptT Text Snap Text
        verilog "verilog" = decodeUtf8 . ByteString.toStrict <$> Snap.readRequestBody (1024 ^ (3 :: Int))
        verilog t         = foundry t >>= ExceptT . return . fmap (genVerilog Map.empty) . parse
        foundry :: ByteString -> ExceptT Text Snap Text
        foundry "defaultFoundry" = liftIO . fmap decodeUtf8 . readFile $ defFn
        foundry "foundry"        = decodeUtf8 . ByteString.toStrict <$> Snap.readRequestBody (1024 ^ (3 :: Int))
        foundry t                = throwError $ "Unrecognised type " ++ tshow t

makeError :: Text -> ByteString
makeError e = "\x01" ++ (encodeWord32 Little . fromIntegral . length . encodeUtf8 $ e) ++ encodeUtf8 e

makeBinReply :: ByteString -> ByteString
makeBinReply bs = "\x00" ++ (encodeWord32 Little . fromIntegral . length $ bs) ++ bs

hostSimulator :: Int -> FilePath -> IO ()
hostSimulator port fn =
  Snap.httpServe (snapConfig port) . asum $
    [ Snap.method Snap.GET . Snap.ifTop $ liftIO (getDataFileName "simulator/index.html" >>= readFile) >>= Snap.writeBS
    , Snap.method Snap.GET . Snap.dir "main.js" . Snap.ifTop $ (liftIO . runExceptT . parseFile $ fn) >>= genSimulatorBS >>= Snap.writeBS
    , Snap.method Snap.GET . Snap.dir "open" . Snap.ifTop $ Snap.getQueryParam "file" >>= maybe Snap.pass (genSimulatorBS . parse . decodeUtf8) >>= Snap.writeBS
    , Snap.method Snap.POST . Snap.dir "burn" . Snap.ifTop $
      runExceptT (synth fn >>= ExceptT . liftIO . runExceptT . burn)
      >>= Snap.writeBS . either makeError (\() -> "\x00")
    , Snap.method Snap.POST . Snap.dir "synth" . Snap.ifTop $
      runExceptT (synth fn)
      >>= Snap.writeBS . either makeError makeBinReply
    ]

hostSimulatorUpdate :: Int -> IO (Either Text Proc -> IO ())
hostSimulatorUpdate port = do
  jsBS <- genSimulatorBS (Left "No file open yet") >>= newTVarIO
  _ <- forkIO . Snap.httpServe (snapConfig port) . asum $
    [ Snap.method Snap.GET . Snap.ifTop $ liftIO (getDataFileName "simulator/index.html" >>= readFile) >>= Snap.writeBS
    , Snap.method Snap.GET . Snap.dir "main.js" . Snap.ifTop $ (liftIO . readTVarIO $ jsBS) >>= Snap.writeBS
    ]
  return (genSimulatorBS >=> atomically . writeTVar jsBS)

