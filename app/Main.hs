{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Main(main) where

import Parser (parseFile)
import CodeGen (genCode)
import Assembler.GenAssembler (genAssembler)

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
import System.FilePath ((-<.>), dropExtensions, takeBaseName, replaceBaseName)

main :: IO ()
main =
  ( runExceptT $ do
    Options{..} <- getOpts
    ast <- parseFile fn
    lift . writeFile (fn -<.> ".v") . genCode $ ast
    when shouldGenAssembler . lift . genAssembler (dropExtensions . replaceBaseName fn $ (takeBaseName fn ++ "_assembler")) $ ast
  ) >>= \case
    Left err -> putStrLn err
    Right () -> return ()

data Options = Options
  { fn :: String
  , shouldGenAssembler :: Bool
  , showHelp :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  ""
  True
  False

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["noAssembler"] (NoArg $ \o -> o { shouldGenAssembler = False }) "Do not generate an assembler"
  , Option ['h'] ["help"] (NoArg $ \o -> o { showHelp = True }) "Print this help message"
  ]

getOpts :: ExceptT String IO Options
getOpts = lift (getOpt Permute options <$> getArgs) >>= \case
  (fs, [fn'], []) -> do
    let Options{..} = foldr ($) (defaultOptions { fn = fn' }) fs
    when showHelp $ throwError "HELP"
    return Options{..}
  (_, _, errs) -> fail . show $ errs
