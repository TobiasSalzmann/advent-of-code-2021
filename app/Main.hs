module Main where

import           Criterion.Main
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
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
  if (head args == "bench")
    then withArgs (drop 1 args) runBench
    else (runDay . read . head) args

runBench :: IO ()
runBench =
  defaultMain [bench ("Day" ++ show d) (nfIO (runDay d)) | d <- [1 .. 13]]

runDay :: Int -> IO ()
runDay 0  = fmap (const ()) . sequence . map runDay $ [1 .. 13]
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
runDay 11 = runConfig Day11.config
runDay 12 = runConfig Day12.config
runDay 13 = runConfig Day13.config
