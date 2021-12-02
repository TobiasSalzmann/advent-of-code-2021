module Main where

import System.Environment
import qualified Day1
import qualified Day2
import Lib

main :: IO ()
main = do
  args <- getArgs
  (runDay . read . head) args
  
runDay :: Int -> IO ()
runDay 1 = runConfig Day1.config
runDay 2 = runConfig Day2.config
  
