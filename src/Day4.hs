{-# LANGUAGE NamedFieldPuns #-}

module Day4
  ( config
  ) where

import           Data.Function   (on)
import           Data.List       (maximumBy, minimumBy, nub, transpose)
import           Data.List.Split (chunksOf)
import           Lib

config :: Config BingoGame Int Int
config =
  Config
    { title = "Day 4"
    , input = fmap parseBingo $ parseIntListLines "resources/day4.txt"
    , part1 = solvePart1
    , part2 = solvePart2
    }

type BingoBoard = [[Int]]

data BingoGame =
  BingoGame
    { numbers :: [Int]
    , boards  :: [BingoBoard]
    }

data ScoreSheet =
  ScoreSheet
    { winningRound :: Int
    , score        :: Int
    }

parseBingo :: [[Int]] -> BingoGame
parseBingo [] = error "unexpected input"
parseBingo (rawNumbers:rawBoards) =
  BingoGame
    {numbers = rawNumbers, boards = chunksOf 5 . filter (/= []) $ rawBoards}

solvePart1 :: BingoGame -> Int
solvePart1 = score . minimumBy (compare `on` winningRound) . scoreSheets

solvePart2 :: BingoGame -> Int
solvePart2 = score . maximumBy (compare `on` winningRound) . scoreSheets

scoreSheets :: BingoGame -> [ScoreSheet]
scoreSheets (BingoGame {numbers, boards}) = map (playBingo numbers) boards

playBingo :: [Int] -> BingoBoard -> ScoreSheet
playBingo numbers board = playBingo' 0 (board ++ transpose board) numbers

playBingo' :: Int -> [[Int]] -> [Int] -> ScoreSheet
playBingo' _ _ [] = error "unexpected case"
playBingo' currentRound rowsAndCols (currentNumber:remainingNumbers) =
  let newRowsAndCols = map (filter (/= currentNumber)) rowsAndCols
   in if [] `elem` newRowsAndCols
        then determineScore currentRound newRowsAndCols currentNumber
        else playBingo' (currentRound + 1) newRowsAndCols remainingNumbers

determineScore :: Int -> [[Int]] -> Int -> ScoreSheet
determineScore currentRound rowsAndCols currentNumber =
  ScoreSheet
    { winningRound = currentRound
    , score = currentNumber * (sum . nub . concat $ rowsAndCols)
    }
