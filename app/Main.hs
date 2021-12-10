module Main where

import qualified Day1
import qualified Day10
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import           Lib
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  (runDay . read . head) args

runDay :: Int -> IO ()
runDay 1  = runConfig Day1.config
runDay 2  = runConfig Day2.config
runDay 3  = runConfig Day3.config
runDay 4  = runConfig Day4.config
runDay 5  = runConfig Day5.config
runDay 6  = runConfig Day6.config
runDay 7  = runConfig Day7.config
runDay 8  = runConfig Day8.config
runDay 9  = runConfig Day9.config
runDay 10 = runConfig Day10.config
