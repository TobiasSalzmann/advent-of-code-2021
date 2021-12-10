{-# LANGUAGE NamedFieldPuns #-}

module Day9
  ( config
  ) where

import           Data.Char (digitToInt)
import           Data.List
import qualified Data.Map  as Map
import           Lib

config :: Config (Map.Map Point2D Int) Int Int
config =
  Config
    { title = "Day 9"
    , input = parseMap digitToInt "resources/day9.txt"
    , part1 = solvePart1
    , part2 = solvePart2
    }

solvePart1 :: Map.Map Point2D Int -> Int
solvePart1 m = sum . map (uncurry (riskLevel m)) . Map.toList $ m

riskLevel :: Map.Map Point2D Int -> Point2D -> Int -> Int
riskLevel m p level
  | all (> level) (manhattanNeighbours m p) = level + 1
  | otherwise = 0

solvePart2 :: Map.Map Point2D Int -> Int
solvePart2 m =
  let initialBasins = Map.mapWithKey const . Map.filter (/= 9) $ m
      finalBasins = mergeBasinsToFixedPoint initialBasins
      basinSizes = map length . group . sort . Map.elems $ finalBasins
   in product . take 3 . reverse . sort $ basinSizes

mergeBasinsToFixedPoint :: Map.Map Point2D Point2D -> Map.Map Point2D Point2D
mergeBasinsToFixedPoint m =
  let nextM = mergeBasins m
   in if nextM == m
        then m
        else mergeBasinsToFixedPoint nextM

mergeBasins :: Map.Map Point2D Point2D -> Map.Map Point2D Point2D
mergeBasins m = Map.mapWithKey (findMinimalBasin m) m

findMinimalBasin :: Map.Map Point2D Point2D -> Point2D -> Point2D -> Point2D
findMinimalBasin m p pBasin = minimum $ pBasin : manhattanNeighbours m p
