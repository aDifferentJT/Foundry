module Main(main) where

import Parser.Parser (parse)
import Parser.LexerMonad (runLexer)

import CodeGen.CodeGen (genCode)

import System.Environment (getArgs)

main :: IO ()
main = do
  --fn <- head <$> getArgs
  let fn = "examples/processor_4.fdry"
  ast <- runLexer parse <$> readFile fn
  case ast of
    Left e      -> putStr e
    Right ast' -> putStr . genCode $ ast'

