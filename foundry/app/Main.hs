{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Main(main) where

import ClassyPrelude hiding (getArgs)

import CallSynth (callSynth)
import GenAssembler (genAssembler)
import GenSimulator (genSimulator, hostSimulator)
import GenVerilog (genVerilog)
import IceBurn (burn)
import Parser (parseFile)

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import System.Console.GetOpt
  ( ArgOrder(Permute)
  , OptDescr(Option)
  , ArgDescr(..)
  , getOpt
  , usageInfo
  )
import System.Environment (getArgs)
import System.FilePath ((-<.>), dropExtensions, takeBaseName, replaceBaseName)
import Text.Read (readMaybe)

main :: IO ()
main = runExceptT 
  ( do
    Options{..} <- getOpts
    ast <- parseFile fn
    let verilog = genVerilog ast
    when shouldGenVerilog . lift . writeFileUtf8 (fn -<.> "v") $ verilog
    when shouldBurn $ (lift . callSynth $ verilog) >>= burn
    when shouldGenAssembler . lift . genAssembler (dropExtensions . replaceBaseName fn $ (takeBaseName fn ++ "_assembler")) $ ast
    lift $ case shouldGenSimulator of
      NoSimulator ->
        return ()
      WriteToFile simFn ->
        genSimulator simFn ast
      HostOnPort p ->
        hostSimulator p ast
    ) >>= \case
    Left err -> putStr err
    Right () -> return ()

data SimulatorDest
  = NoSimulator
  | WriteToFile FilePath
  | HostOnPort Int

data Options = Options
  { fn :: FilePath
  , shouldGenVerilog :: Bool
  , shouldBurn :: Bool
  , shouldGenAssembler :: Bool
  , shouldGenSimulator :: SimulatorDest
  , showHelp :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  ""
  False
  False
  False
  NoSimulator
  False

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verilog"] (NoArg $ \o -> o { shouldGenVerilog = True }) "Generate the verilog"
  , Option ['b'] ["burn"] (NoArg $ \o -> o { shouldBurn = True }) "Burn to the board"
  , Option ['a'] ["assembler"] (NoArg $ \o -> o { shouldGenAssembler = True }) "Generate an assembler"
  , Option ['s'] ["simulator"] (ReqArg (\a o -> o { shouldGenSimulator = maybe (WriteToFile a) HostOnPort . readMaybe $ a }) "<file|port>")
    "Generate a simulator then, if a file is given, write to the specified file, if a port is given then host"
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

