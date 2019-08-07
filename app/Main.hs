{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Main(main) where

import Parser (parseFile)
import CodeGen (genCode)

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import System.Console.GetOpt
  ( getOpt
  , ArgOrder(Permute)
  , OptDescr(Option)
  , ArgDescr(NoArg)
  )
import System.Environment (getArgs)
import System.FilePath ((-<.>))

main :: IO ()
main = runExceptT (
  do
    Options{..} <- getOpts
    ast <- parseFile fn
    lift . writeFile (fn -<.> ".v") . genCode $ ast
  ) >>= \case
    Left err -> putStrLn err
    Right () -> return ()

data Options = Options
  { fn :: String
  , showHelp :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  ""
  False

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"] (NoArg $ \o -> o { showHelp = True }) "Print this help message"
  ]

getOpts :: ExceptT String IO Options
getOpts = lift (getOpt Permute options <$> getArgs) >>= \case
  (fs, [fn'], []) -> do
    let Options{..} = foldr ($) (defaultOptions { fn = fn' }) fs
    when showHelp $ throwError "HELP"
    return Options{..}
  (_, _, errs) -> fail . show $ errs
