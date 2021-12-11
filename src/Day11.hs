{-# LANGUAGE NamedFieldPuns #-}

module Day11
  ( config
  ) where

import           Data.Char (digitToInt)
import qualified Data.Map  as Map
import           Lib

config :: Config (Map.Map Point2D Int) Int Int
config =
  Config
    { title = "Day 11"
    , input = parseMap digitToInt "resources/day11.txt"
    , part1 = solvePart1
    , part2 = solvePart2
    }

solvePart1 :: Map.Map Point2D Int -> Int
solvePart1 = countFlashes 100 0

countFlashes :: Int -> Int -> Map.Map Point2D Int -> Int
countFlashes 0 flashes _ = flashes
countFlashes rounds flashes m =
  let (newFlashes, newM) = executeRound m
   in countFlashes (rounds - 1) (flashes + newFlashes) newM

executeFlashes ::
     [Point2D] -> Map.Map Point2D Int -> ([Point2D], Map.Map Point2D Int)
executeFlashes flashed m =
  let (toFlash, rest) = Map.partition (> 9) m
   in if Map.null toFlash
        then (flashed, m)
        else let pointsAboutToFlash = Map.keys toFlash
                 pointsToIncrement = concatMap neighbours pointsAboutToFlash
                 updated =
                   foldl
                     (\acc p -> Map.update (Just . (+ 1)) p acc)
                     rest
                     pointsToIncrement
              in executeFlashes (flashed ++ pointsAboutToFlash) updated

executeRound :: Map.Map Point2D Int -> (Int, Map.Map Point2D Int)
executeRound m =
  let m' = Map.map (+ 1) m
      (flashed, remaining) = executeFlashes [] m'
      updatedM = Map.union remaining (Map.fromList (flashed `zip` [0,0 ..]))
   in (length flashed, updatedM)

solvePart2 :: Map.Map Point2D Int -> Int
solvePart2 = countRoundsToSync 0

countRoundsToSync :: Int -> Map.Map Point2D Int -> Int
countRoundsToSync pastRounds m =
  let (newFlashes, newM) = executeRound m
      currentRound = pastRounds + 1
   in if newFlashes == length m
        then currentRound
        else countRoundsToSync currentRound newM
