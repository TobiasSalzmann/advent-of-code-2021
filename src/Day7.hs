module Day7
  ( config
  , solve
  ) where

import           Lib

config :: Config [Int] Int Int
config =
  Config
    { title = "Day 7"
    , input = fmap head $ parseIntListLines "resources/day7.txt"
    , part1 = solve (\x y -> abs (x - y))
    , part2 = solve (\x y -> abs (x - y) * (abs (x - y) + 1) `div` 2)
    }

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve fc xs = minimum . map (totalDistance fc xs) $ [minimum xs .. maximum xs]

totalDistance :: (Int -> Int -> Int) -> [Int] -> Int -> Int
totalDistance fc xs x = sum . map (fc x) $ xs
