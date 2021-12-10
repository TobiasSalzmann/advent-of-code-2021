module Day3
  ( config
  ) where

import           Data.Char (digitToInt)
import           Data.List (foldl', transpose)
import           Lib

type Bit = Int

type BitString = [Bit]

config :: Config [BitString] Integer Integer
config =
  Config
    { title = "Day 3"
    , input = Lib.parseLinesWith (map digitToInt) "resources/day3.txt"
    , part1 = part1Sol
    , part2 = part2Sol
    }

part1Sol :: [BitString] -> Integer
part1Sol measurements =
  let cols = transpose measurements
      rawGamma = map dominantBit cols
      rawEpsilon = map (1 -) rawGamma
   in bitsToInteger rawGamma * bitsToInteger rawEpsilon

part2Sol :: [BitString] -> Integer
part2Sol measurements =
  let oxygen = select dominantBit measurements
      carbon = select ((1 -) . dominantBit) measurements
   in bitsToInteger oxygen * bitsToInteger carbon

select :: ([Bit] -> Bit) -> [BitString] -> BitString
select _ [singleton] = singleton
select selector measurements =
  let heads = map head measurements
      selected = selector heads
      filtered = filter ((selected ==) . head) measurements
   in selected : select selector (map tail filtered)

bitsToInteger :: [Bit] -> Integer
bitsToInteger = foldl' (\acc x -> acc * 2 + fromIntegral x) 0

dominantBit :: [Bit] -> Bit
dominantBit bs =
  if 2 * (sum bs) >= length bs
    then 1
    else 0
