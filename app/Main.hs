{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Main(main) where

import ClassyPrelude hiding (getArgs)

import Parser (parseFile)
import CodeGen (genCode)
import Assembler.GenAssembler (genAssembler)
import Simulator.GenSimulator (genSimulator)
import CallSynth (callSynth)
import IceBurn (burn)

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import System.Console.GetOpt
  ( ArgOrder(Permute)
  , OptDescr(Option)
  , ArgDescr(NoArg)
  , getOpt
  , usageInfo
  )
import System.Environment (getArgs)
import System.FilePath ((-<.>), dropExtensions, takeBaseName, replaceBaseName)

main :: IO ()
main = runExceptT 
  ( do
    Options{..} <- getOpts
    ast <- parseFile fn
    let verilog = genCode ast
    when shouldGenVerilog . lift . writeFileUtf8 (fn -<.> "v") $ verilog
    when shouldGenAssembler . lift . genAssembler (dropExtensions . replaceBaseName fn $ (takeBaseName fn ++ "_assembler")) $ ast
    when shouldGenSimulator . lift . genSimulator (fn -<.> "html") $ ast
    when shouldBurn $ (lift . callSynth $ verilog) >>= burn
    ) >>= \case
    Left err -> putStr err
    Right () -> return ()

data Options = Options
  { fn :: FilePath
  , shouldGenVerilog :: Bool
  , shouldBurn :: Bool
  , shouldGenAssembler :: Bool
  , shouldGenSimulator :: Bool
  , showHelp :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  ""
  False
  False
  False
  False
  False

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verilog"] (NoArg $ \o -> o { shouldGenVerilog = True }) "Generate the verilog"
  , Option ['b'] ["burn"] (NoArg $ \o -> o { shouldBurn = True }) "Burn to the board"
  , Option ['a'] ["assembler"] (NoArg $ \o -> o { shouldGenAssembler = True }) "Generate an assembler"
  , Option ['s'] ["simulator"] (NoArg $ \o -> o { shouldGenSimulator = True }) "Generate a simulator"
  , Option ['h'] ["help"] (NoArg $ \o -> o { showHelp = True }) "Print this help message"
  ]

getOpts :: ExceptT Text IO Options
getOpts = lift (getOpt Permute options <$> getArgs) >>= \case
  (fs, fns, []) -> do
    let Options{..} = foldr ($) defaultOptions fs
    when showHelp
      . throwError
      . pack
      . flip usageInfo options
      $ "Usage:\n    foundry [options] <filename>\n"
    fn <- case fns of
      []    -> throwError "Missing filename\n"
      [fn'] -> return fn'
      _     -> throwError "Multiple filenames\n"
    return Options{..}
  (_, _, errs) -> throwError . concatMap pack $ errs

