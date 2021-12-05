module Main where

import System.Environment
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import Lib

main :: IO ()
main = do
  args <- getArgs
  (runDay . read . head) args

runDay :: Int -> IO ()
runDay 1 = runConfig Day1.config
runDay 2 = runConfig Day2.config
runDay 3 = runConfig Day3.config
runDay 4 = runConfig Day4.config
runDay 5 = runConfig Day5.config

