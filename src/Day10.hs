{-# LANGUAGE NamedFieldPuns #-}

module Day10 where

import           Data.Char   (digitToInt)
import           Data.Either (lefts, rights)
import           Data.List
import qualified Data.Map    as Map
import           Lib

config :: Config [String] Int Integer
config =
  Config
    { title = "Day 10"
    , input = parseLinesWith id "resources/day10.txt"
    , part1 = solvePart1
    , part2 = solvePart2
    }

solvePart1 :: [String] -> Int
solvePart1 = sum . map score . lefts . map (check "")

solvePart2 :: [String] -> Integer
solvePart2 = middle . map (score2 0) . rights . map (check "")

check :: String -> String -> Either String String
check acc ('(':cs) = check (')' : acc) cs
check acc ('[':cs) = check (']' : acc) cs
check acc ('{':cs) = check ('}' : acc) cs
check acc ('<':cs) = check ('>' : acc) cs
check (c1:acc) (c2:cs)
  | c1 == c2 = check acc cs
check acc [] = Right acc
check _ cs = Left cs

middle :: (Ord a) => [a] -> a
middle xs = sort xs !! (length xs `div` 2)

score :: String -> Int
score (')':_) = 3
score (']':_) = 57
score ('}':_) = 1197
score ('>':_) = 25137
score _       = 0

score2 :: Integer -> String -> Integer
score2 total (')':xs) = score2 (total * 5 + 1) xs
score2 total (']':xs) = score2 (total * 5 + 2) xs
score2 total ('}':xs) = score2 (total * 5 + 3) xs
score2 total ('>':xs) = score2 (total * 5 + 4) xs
score2 total []       = total
score2 _ _            = error "strange"
