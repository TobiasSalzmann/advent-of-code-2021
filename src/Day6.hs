{-# LANGUAGE NamedFieldPuns #-}

module Day6
  ( config
  ) where

import qualified Data.Map as Map
import           Lib

config :: Config (Map.Map Int Integer) Integer Integer
config =
  Config
    { title = "Day 6"
    , input =
        fmap (Map.fromListWith (+) . (`zip` [1,1 ..]) . head) $
        parseIntListLines "resources/day6.txt"
    , part1 = solve 80
    , part2 = solve 256
    }

solve :: Int -> Map.Map Int Integer -> Integer
solve n = sum . Map.elems . (!! n) . iterate step

step :: Map.Map Int Integer -> Map.Map Int Integer
step pop =
  let aboutToReplicate = Map.findWithDefault 0 0 pop
      aged = Map.mapKeysWith (+) age pop
   in Map.insert 8 aboutToReplicate aged

age :: Int -> Int
age 0 = 6
age n = n - 1
