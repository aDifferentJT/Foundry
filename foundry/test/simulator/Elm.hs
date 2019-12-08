module Main (main) where

import System.Directory (getCurrentDirectory)
import System.Exit (exitWith)
import System.FilePath ((</>))
import System.IO (putStrLn)
import System.Process (createProcess, cwd, proc, waitForProcess)

main :: IO ()
main = do
    putStrLn "" -- less confusing output, test-framework does this too
    dir <- (</> "simulator") <$> getCurrentDirectory
    (_, _, _, ph) <- createProcess $ (proc "elm-test" []) { cwd = Just dir }
    exitWith =<< waitForProcess ph

