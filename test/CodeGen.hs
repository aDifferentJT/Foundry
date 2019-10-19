{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude hiding (getArgs)

import Parser (parse)
import CodeGen (genCode)

import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  fn <- fromMaybe "examples/processor_4_reg.fdry" . listToMaybe <$> getArgs
  ast <- parse <$> readFileUtf8 fn
  case ast of
    Left e      -> putStr e
    Right ast' -> putStr . genCode $ ast'

