module Main(main) where

import Parser.Parser (parse)
import Parser.LexerMonad (runLexer)
import Parser.TypeChecker (typeCheck)

import System.Environment (getArgs)

main :: IO ()
main = do
  fn <- head <$> getArgs
  ast <- runLexer parse <$> readFile fn
  let ast' = ast >>= typeCheck
  case ast' of
    Left e      -> putStr e
    Right ast'' -> print ast''

