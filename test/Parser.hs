module Main(main) where

import Parser (parse)

import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  fn <- fromMaybe "examples/processor_4.fdry" . listToMaybe <$> getArgs
  ast <- parse <$> readFile fn
  case ast of
    Left e     -> putStr e
    Right ast' -> print ast'

