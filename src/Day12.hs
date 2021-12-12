{-# LANGUAGE NamedFieldPuns #-}

module Day12
  ( config
  ) where

import           Data.Char (isUpper)
import           Data.List
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           Lib

config :: Config [(String, String)] Int Int
config =
  Config
    { title = "Day 12"
    , input = parseLinesWith parseEdge "resources/day12.txt"
    , part1 = solvePart1
    , part2 = solvePart2
    }

type Node = String

type Edge = (String, String)

parseEdge :: String -> (String, String)
parseEdge s =
  let (a, '-':b) = span (/= '-') s
   in (a, b)

type Graph = Map.Map Node [Node]

solvePart1 :: [Edge] -> Int
solvePart1 = countPaths Set.empty False "start" "end" . parseGraph

solvePart2 :: [Edge] -> Int
solvePart2 = countPaths Set.empty True "start" "end" . parseGraph

parseGraph :: [Edge] -> Graph
parseGraph = Map.fromListWith (++) . concatMap (\(a, b) -> [(a, [b]), (b, [a])])

countPaths :: Set.Set Node -> Bool -> Node -> Node -> Graph -> Int
countPaths visited canVisitAgain start end graph
  | start == end = 1
  | otherwise =
    let adj = graph Map.! start
        validNext = filter (\n -> isBigCave n || notElem n visited) $ adj
        validNextSpecial =
          filter
            (\n ->
               notElem n validNext &&
               canVisitAgain && n /= "start" && n /= "end") $
          adj
        nextVisited = Set.insert start visited
     in sum .
        map 
          (\n ->
             countPaths
               nextVisited
               (canVisitAgain && notElem n validNextSpecial)
               n
               end
               graph) $
        (validNext ++ validNextSpecial)

isBigCave :: Node -> Bool
isBigCave = all isUpper
