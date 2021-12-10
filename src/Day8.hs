{-# LANGUAGE NamedFieldPuns #-}

module Day8
  ( config
  ) where

import           Data.Char  (intToDigit, ord)
import           Data.List
import           Data.Maybe (fromJust)
import qualified Data.Set   as Set
import           Lib

config :: Config [Display] Int Int
config =
  Config
    { title = "Day 8"
    , input = fmap (map parseDisplay) $ parseWordLists "resources/day8.txt"
    , part1 = solvePart1
    , part2 = solvePart2
    }

data Display =
  Display
    { patterns :: [String]
    , output   :: [String]
    }

parseDisplay :: [String] -> Display
parseDisplay inp =
  case span (/= "|") inp of
    (pat, _:out) -> Display {patterns = pat, output = out}
    _            -> error "invalid input"

solvePart1 :: [Display] -> Int
solvePart1 =
  length . filter (`elem` [2, 3, 4, 7]) . map length . concatMap output

solvePart2 :: [Display] -> Int
solvePart2 = sum . (map readDisplay)

readDisplay :: Display -> Int
readDisplay (Display {patterns, output}) =
  readDisplayUsingPerm (deducePerm patterns) output

readDisplayUsingPerm :: String -> [String] -> Int
readDisplayUsingPerm perm =
  read . map (intToDigit . fromJust . (`elemIndex` digits) . applyPerm perm)

digits :: [String]
digits =
  [ "abcefg"
  , "cf"
  , "acdeg"
  , "acdfg"
  , "bcdf"
  , "abdfg"
  , "abdefg"
  , "acf"
  , "abcdefg"
  , "abcdfg"
  ]

applyPerm :: String -> String -> String
applyPerm perm = sort . map (toFunc perm)

toFunc :: String -> Char -> Char
toFunc perm c = perm !! (ord c - ord 'a')

deducePerm :: [String] -> String
deducePerm patterns =
  let [pat2, pat3, pat4, _, _, _, pat61, pat62, pat63, pat7] =
        sortOn length patterns
      cf = pat2
      a = pat3 \\ cf
      bd = pat4 \\ cf
      eg = pat7 \\ (cf ++ a ++ bd)
      [g] =
        [y | x <- [pat61, pat62, pat63], let y = intersect eg x, length y == 1]
      [f] =
        filter ((== 1) . length) . map (intersect cf) $ [pat61, pat62, pat63]
      [b] =
        filter ((== 1) . length) . map (intersect bd) $ [pat61, pat62, pat63]
   in invertPerm . concat $ [a, b, cf \\ f, bd \\ b, eg \\ g, f, g]

--      [g] = filter ((==1) . length). map (intersect eg ) $ [pat61, pat62, pat63]
invertPerm :: String -> String
invertPerm = map fst . sortOn snd . zip "abcdefg"
