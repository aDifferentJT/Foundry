{-# LANGUAGE LambdaCase, MultiWayIf, NoImplicitPrelude, OverloadedStrings, TupleSections, ViewPatterns #-}

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
import qualified Snap.Util.FileUploads as Snap
import System.Directory (getModificationTime)
import System.IO.Temp (openTempFile)

snapConfig :: Int -> Snap.Config Snap a
snapConfig port =
  Snap.setAccessLog Snap.ConfigNoLog
  . Snap.setErrorLog Snap.ConfigNoLog
  . Snap.setPort port
  $ mempty

getQueryParam :: ByteString -> ExceptT Text Snap ByteString
getQueryParam name = ExceptT $ maybe (Left $ "Could not get \"" ++ decodeUtf8 name ++ "\" from request") Right <$> Snap.getQueryParam name

firstM :: Functor m => (a1 -> m a2) -> (a1, b) -> m (a2, b)
firstM f (x, y) = (,y) <$> f x

secondM :: Functor m => (b1 -> m b2) -> (a, b1) -> m (a, b2)
secondM f (x, y) = (x,) <$> f y

untilMaybe :: Monad m => (a -> Maybe b) -> (a -> m a) -> a -> m b
untilMaybe f g x = case f x of
  Nothing -> g x >>= untilMaybe f g
  Just y  -> return y

writeTempFile :: FilePath -> Text -> ByteString -> IO FilePath
writeTempFile dir name bs = do
  (fn, h) <- openTempFile dir (unpack name)
  hPut h bs
  return fn

data FileType
  = Bin ByteString
  | Verilog Text
  | Foundry Text

synth :: FilePath -> ExceptT Text Snap ByteString
synth defFn = do
  params <- (uncurry (++) . second (map (\(Snap.FormFile name value) -> (name, value))) <$> Snap.handleFormUploads
    Snap.defaultUploadPolicy
    Snap.defaultFileUploadPolicy
    (const (fmap ByteString.toStrict . Snap.storeAsLazyByteString))
    ) :: ExceptT Text Snap [(ByteString, ByteString)]
  file <-
    let fileBS = maybe (throwError "No file in HTTP Request Body") return . lookup "file" $ params in
    getQueryParam "type" >>= \case
      "bin"            -> Bin <$> fileBS
      "verilog"        -> Verilog . decodeUtf8 <$> fileBS
      "defaultFoundry" -> Foundry . decodeUtf8 <$> liftIO (readFile defFn)
      "foundry"        -> Foundry . decodeUtf8 <$> fileBS
      t                -> throwError $ "Unrecognised type " ++ tshow t
  ExceptT . liftIO . withSystemTempDirectory "foundry_mems" $ \dir -> runExceptT $ do
    mems <- lift
      . fmap Map.fromList
      . mapM (\(name, contents) -> (name,) <$> writeTempFile dir name contents)
      . mapMaybe (firstM ((stripPrefix "mem_" :: Text -> Maybe Text) . decodeUtf8))
      $ params
    regDefs <-
      ExceptT
      . return
      . fmap Map.fromList
      . mapM (secondM (maybe (throwError "Register value not an integer") return . (readMay :: Text -> Maybe Int) . decodeUtf8))
      . mapMaybe (firstM (stripPrefix "reg_" . decodeUtf8))
      $ params
    untilMaybe
      (\case
        Bin bs -> Just bs
        _      -> Nothing
      )
      (\case
        Bin contents     -> return $ Bin contents
        Verilog contents -> lift $ Bin <$> callSynth contents
        Foundry contents -> fmap (Verilog . genVerilog mems regDefs) . ExceptT . return $ parse contents
      )
      file

makeError :: Text -> ByteString
makeError e = "\x01" ++ (encodeWord32 Little . fromIntegral . length . encodeUtf8 $ e) ++ encodeUtf8 e

makeBinReply :: ByteString -> ByteString
makeBinReply bs = "\x00" ++ (encodeWord32 Little . fromIntegral . length $ bs) ++ bs

hostSimulator :: Int -> FilePath -> IO ()
hostSimulator port fn = do
  refCacheTime <- getModificationTime fn >>= newIORef
  cache <- (runExceptT . parseFile $ fn) >>= genSimulatorBS >>= newIORef
  Snap.httpServe (snapConfig port) . asum $
    [ Snap.method Snap.GET . Snap.ifTop $ liftIO (getDataFileName "simulator/index.html" >>= readFile) >>= Snap.writeBS
    , Snap.method Snap.GET . Snap.dir "iceburn.js" . Snap.ifTop $ do
      Snap.modifyResponse (Snap.setContentType "application/javascript")
      liftIO (getDataFileName "simulator/iceburn/iceburn.js" >>= readFile) >>= Snap.writeBS
    , Snap.method Snap.GET . Snap.dir "main.js" . Snap.ifTop $ do
      Snap.modifyResponse (Snap.setContentType "application/javascript")
      Snap.getQueryParam "file" >>= maybe
        (liftIO $ do
          modTime <- getModificationTime fn
          cacheTime <- readIORef refCacheTime
          if modTime <= cacheTime then
            readIORef cache
          else do
            bs <- (runExceptT . parseFile $ fn) >>= genSimulatorBS
            writeIORef refCacheTime modTime
            writeIORef cache bs
            return bs
        ) (genSimulatorBS . parse . decodeUtf8) >>= Snap.writeBS
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

