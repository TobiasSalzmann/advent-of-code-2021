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

data ShipState =
  ShipState
    { visited          :: Set.Set Node
    , allowDoubleVisit :: Bool
    , cave             :: Node
    }
  deriving (Eq, Ord)

parseGraph :: [Edge] -> Graph
parseGraph = Map.fromListWith (++) . concatMap (\(a, b) -> [(a, [b]), (b, [a])])

solvePart :: Bool -> [Edge] -> Int
solvePart allowDoubleVisit e =
  Dag.countPaths
    ((== "end") . cave)
    (validNext' (parseGraph e))
    ShipState {visited = Set.empty, allowDoubleVisit, cave = "start"}

isSmallCave :: Node -> Bool
isSmallCave = all isLower

validNext' :: Graph -> ShipState -> [ShipState]
validNext' graph (s@ShipState {visited, allowDoubleVisit, cave})
  | cave `elem` visited &&
      isSmallCave cave && cave /= "start" && allowDoubleVisit =
    next graph (s {allowDoubleVisit = False})
  | cave `elem` visited && isSmallCave cave = []
  | otherwise = next graph s

next :: Graph -> ShipState -> [ShipState]
next graph (ShipState {visited, allowDoubleVisit, cave}) =
  [ ShipState {visited = Set.insert cave visited, allowDoubleVisit, cave = n}
  | n <- graph ! cave
  ]
