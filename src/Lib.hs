{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Data.List (dropWhileEnd, foldl')
import Data.List.Split (wordsBy)
import Data.Char
import Data.Maybe(mapMaybe)
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = putStrLn "someFunc"
  
parseIntLines :: String -> IO [Int]
parseIntLines = parseLinesWith read

parseWordLists :: String -> IO [[String]]
parseWordLists = parseLinesWith words

parseIntListLines :: String -> IO[[Int]]
parseIntListLines = parseLinesWith (map read . wordsBy (not . isNumber))

data Point2D = Point2D{xCoord :: Int, yCoord :: Int} deriving (Eq, Ord)

parseMap :: (Char -> a) -> String -> IO (Map.Map Point2D a)
parseMap f fileName = do 
  mapLines <- parseLinesWith id fileName
  return $ Map.fromList [(Point2D{xCoord=x, yCoord=y}, f c) | (y, line) <- [0..] `zip` mapLines, (x, c) <- [0..] `zip` line]
  
manhattanNeighbours :: Map.Map Point2D a -> Point2D -> [a]
manhattanNeighbours m (p@Point2D{xCoord, yCoord}) = mapMaybe id [m Map.!? p {xCoord=xCoord+1}, m Map.!? p {xCoord=xCoord-1}, m Map.!? p {yCoord=yCoord+1}, m Map.!? p {yCoord=yCoord-1}]   
  
parseLinesWith :: (String -> a) -> String -> IO [a]
parseLinesWith f fileName = do
  raw <- readFile fileName
  let rawLines = (lines . dropWhileEnd isSpace) raw 
  return (map f rawLines)
  
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