module Day1
  ( config
  ) where

import           Lib

config :: Config [Int] Int Int
config =
  Config
    { title = "Day 1"
    , input = Lib.parseIntLines "resources/day1.txt"
    , part1 = countIncreases 1
    , part2 = countIncreases 3
    }

countIncreases :: Int -> [Int] -> Int
countIncreases dist = length . filter (uncurry (<)) . pairs dist

pairs :: Int -> [a] -> [(a, a)]
pairs dist xs = xs `zip` drop dist xs
