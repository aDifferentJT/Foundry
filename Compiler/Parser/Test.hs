module Main(main) where

import Parser (parse)
import LexerMonad (runLexer)
import TypeChecker (typeCheck)

import System.Environment (getArgs)

main :: IO ()
main = do
  fn <- head <$> getArgs
  ast <- runLexer parse <$> readFile fn
  let ast' = ast >>= typeCheck
  case ast' of
    Left e      -> putStrLn e
    Right ast'' -> print ast''

