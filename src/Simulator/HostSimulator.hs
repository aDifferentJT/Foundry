{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-|
Module      : Simulator.HostSimulator
Description : Host a web based simulator
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Simulator.HostSimulator
  ( hostSimulator
  ) where

import ClassyPrelude

import Control.Concurrent (ThreadId, forkIO)
import Snap.Core
import Snap.Http.Server

import Proc (Proc)
import Simulator.GenSimulator

hostSimulator :: Int -> Proc -> IO ()
hostSimulator port ast = do
  htmlBS <- withSystemTempFile "index.html" $ \tmpFn tmpH -> genSimulator tmpFn ast >> hGetContents tmpH
  httpServe snapConfig $
    ifTop (writeBS htmlBS)
  where snapConfig :: Config Snap a
        snapConfig =
          setAccessLog ConfigNoLog
          . setErrorLog ConfigNoLog
          . setPort port
          $ mempty

