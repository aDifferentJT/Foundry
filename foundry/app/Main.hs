{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Main(main) where

import ClassyPrelude hiding (getArgs)

import CallSynth (burn)
import GenAssembler (genAssembler)
import GenSimulator (genElm, hostSimulator, runElm)
import GenVerilog (genVerilog)
import Language.Foundry.Parser (parseFile)

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (breakOn)
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
    let verilog = genVerilog memoryFiles ast
    when shouldGenVerilog . lift . writeFileUtf8 (fn -<.> "v") $ verilog
    when shouldBurn . burn $ verilog
    when shouldGenAssembler . lift . genAssembler (dropExtensions . replaceBaseName fn $ (takeBaseName fn ++ "_assembler")) $ ast
    lift $ case shouldGenSimulator of
      NoSimulator ->
        return ()
      WriteToFile simFn ->
        runElm simFn . genElm $ ast
      HostOnPort p ->
        hostSimulator p fn (burn . genVerilog memoryFiles)
    ) >>= \case
    Left err -> putStr err
    Right () -> return ()

data SimulatorDest
  = NoSimulator
  | WriteToFile FilePath
  | HostOnPort Int

data Options = Options
  { fn :: FilePath
  , memoryFiles :: Map Text FilePath
  , shouldGenVerilog :: Bool
  , shouldBurn :: Bool
  , shouldGenAssembler :: Bool
  , shouldGenSimulator :: SimulatorDest
  , showHelp :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  ""
  Map.empty
  False
  False
  False
  NoSimulator
  False

options :: [OptDescr (Options -> Options)]
options =
  [ Option
    ['v']
    ["verilog"]
    ( NoArg $ \o -> o
      { shouldGenVerilog = True
      }
    )
    "Generate the verilog"
  , Option
    ['b']
    ["burn"]
    ( NoArg $ \o -> o
      { shouldBurn = True
      }
    )
    "Burn to the board"
  , Option
    ['a']
    ["assembler"]
    ( NoArg $ \o -> o
      { shouldGenAssembler = True
      }
    )
    "Generate an assembler"
  , Option
    ['s']
    ["simulator"]
    ( ReqArg
      (\a o -> o
        { shouldGenSimulator =
          maybe (WriteToFile a) HostOnPort . readMaybe $ a
        }
      )
      "<file|port>"
    )
    "Generate a simulator then, if a file is given, write to the specified file, if a port is given then host"
  , Option
    ['m']
    ["memory"]
    ( ReqArg
      (\a o -> o
        { memoryFiles =
          let (mem, file) = second (fromMaybe "" . tailMay . unpack) . breakOn "=" . pack $ a in
          Map.insert mem file . memoryFiles $ o
        }
      )
      "<mem>=<file>"
    )
    "Fill one of the memories with the contents of the given file"
  , Option
    ['h']
    ["help"]
    ( NoArg $ \o -> o
      { showHelp = True
      }
    )
    "Print this help message"
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
    fn' <- case fns of
      []     -> throwError "Missing filename\n"
      [fn''] -> return fn''
      _      -> throwError "Multiple filenames\n"
    return Options{ fn = fn', .. }
  (_, _, errs) -> throwError . concatMap pack $ errs

