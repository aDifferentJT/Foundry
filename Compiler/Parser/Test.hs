module Main(main) where

import Parser.Parser (parse)
import Parser.LexerMonad (runLexer)

import System.Environment (getArgs)

main :: IO ()
main = do
  fn <- head <$> getArgs
  ast <- runLexer parse <$> readFile fn
  case ast of
    Left e     -> putStr e
    Right ast' -> print ast'

