{-# LANGUAGE NamedFieldPuns #-}

module Day12
  ( config
  ) where

import qualified Dag
import           Data.Char (isLower, isUpper)
import           Data.List
import           Data.Map  ((!))
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           Lib

config :: Config [(String, String)] Int Int
config =
  Config
    { title = "Day 12"
    , input = parseLinesWith parseEdge "resources/day12.txt"
    , part1 = solvePart False
    , part2 = solvePart True
    }

type Node = String

type Edge = (String, String)

parseEdge :: String -> (String, String)
parseEdge s =
  let (a, '-':b) = span (/= '-') s
   in (a, b)

type Graph = Map.Map Node [Node]

data ShipState = ShipState {visited :: Set.Set Node
,allowDoubleVisit :: Bool
,cave :: Node
}

parseGraph :: [Edge] -> Graph
parseGraph = Map.fromListWith (++) . concatMap (\(a, b) -> [(a, [b]), (b, [a])])

solvePart :: Bool -> [Edge] -> Int
solvePart allowDoubleVisit e =
  Dag.countPaths
    (\(_, _, n) -> n == "end")
    (validNext' (parseGraph e))
    (Set.empty, allowDoubleVisit, "start")

isSmallCave :: Node -> Bool
isSmallCave = all isLower

validNext' ::
     Graph -> (Set.Set Node, Bool, Node) -> [(Set.Set Node, Bool, Node)]
validNext' graph (visited, canVisitTwice, node)
  | node `elem` visited && isSmallCave node && node /= "start" && canVisitTwice =
    [(Set.insert node visited, False, n) | n <- graph ! node]
  | node `elem` visited && isSmallCave node = []
  | otherwise =
    [(Set.insert node visited, canVisitTwice, n) | n <- graph ! node]
