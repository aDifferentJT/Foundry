{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Exit (ExitCode(ExitFailure), exitWith)

main :: IO ()
main = do
  print "Hi"
  exitWith (ExitFailure 1)

