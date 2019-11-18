module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (putStrLn)

import Language.Haskell.HLint (hlint)

main :: IO ()
main = do
    putStrLn "" -- less confusing output, test-framework does this too
    hints <- hlint ["."]
    unless (null hints) exitFailure

