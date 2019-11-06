{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Main(main) where

import ClassyPrelude hiding (getArgs)

import Parser (parseFile)
import CodeGen (genCode)
import Assembler.GenAssembler (genAssembler)
import CallSynth (callSynth)
import IceBurn (burn)

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
main = runExceptT 
  ( do
    Options{..} <- getOpts
    ast <- parseFile fn
    let verilog = genCode ast
    when shouldGenVerilog . lift . writeFileUtf8 (fn -<.> ".v") $ verilog
    when shouldGenAssembler . lift . genAssembler (dropExtensions . replaceBaseName fn $ (takeBaseName fn ++ "_assembler")) $ ast
    when shouldBurn $ (lift . callSynth $ verilog) >>= burn
    ) >>= \case
    Left err -> putStrLn err
    Right () -> return ()

data Options = Options
  { fn :: FilePath
  , shouldGenVerilog :: Bool
  , shouldBurn :: Bool
  , shouldGenAssembler :: Bool
  , showHelp :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  ""
  False
  False
  False
  False

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verilog"] (NoArg $ \o -> o { shouldGenVerilog = True }) "Generate the verilog"
  , Option ['b'] ["burn"] (NoArg $ \o -> o { shouldBurn = True }) "Burn to the board"
  , Option ['a'] ["assembler"] (NoArg $ \o -> o { shouldGenAssembler = True }) "Generate an assembler"
  , Option ['h'] ["help"] (NoArg $ \o -> o { showHelp = True }) "Print this help message"
  ]

getOpts :: ExceptT Text IO Options
getOpts = lift (getOpt Permute options <$> getArgs) >>= \case
  (fs, [fn'], []) -> do
    let Options{..} = foldr ($) (defaultOptions { fn = fn' }) fs
    when showHelp $ throwError "HELP"
    return Options{..}
  (_, _, errs) -> fail . show $ errs
