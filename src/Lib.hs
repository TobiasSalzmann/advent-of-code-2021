{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Data.List.Split (splitOn)
import Data.List (dropWhileEnd)
import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

parseStrings :: String -> IO [String]
parseStrings fileName = do
  raw <- readFile fileName
  (return . splitOn ", " . dropWhileEnd isSpace) raw
  
parseIntLines :: String -> IO [Int]
parseIntLines fileName = do
  raw <- readFile fileName
  (return . map read . lines . dropWhileEnd isSpace) raw
  
succR :: (Eq a, Bounded a , Enum a) => a -> a
succR x 
  | x == maxBound = minBound
  | otherwise = succ x

predR :: (Eq a, Bounded a , Enum a) => a -> a
predR x 
  | x == minBound = maxBound
  | otherwise = pred x
  
run :: (Show r1, Show r2) => String -> IO a -> (a -> r1) -> (a -> r2) -> IO ()
run title parse part1 part2 = do
  parsed <- parse
  putStrLn (title ++ ":")
  putStrLn ("Part 1:")
  (putStrLn . show . part1) parsed
  putStrLn ("Part 2:")
  (putStrLn . show . part2) parsed
    
data Config a r1 r2 = Config  { title :: String  
                     , input :: IO a  
                     , part1 :: a -> r1
                     , part2 :: a -> r2
                     }  

runConfig :: (Show r1, Show r2) => Config a r1 r2 -> IO ()
runConfig (Config {title, input, part1, part2}) = do
                                                    parsed <- input
                                                    putStrLn (title ++ ":")
                                                    putStrLn ("Part 1:")
                                                    (putStrLn . show . part1) parsed
                                                    putStrLn ("Part 2:")
                                                    (putStrLn . show . part2) parsed