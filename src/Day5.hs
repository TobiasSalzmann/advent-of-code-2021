{-# LANGUAGE NamedFieldPuns #-}
module Day5 (config) where
  
import Lib
import qualified Data.MultiSet as MultiSet

config :: Config [LineSegment] Int Int
config = Config {title = "Day 5"
                ,input = fmap (map parseLineSegment) $ parseIntListLines "resources/day5.txt"
                ,part1 = solvePart1
                ,part2 = solvePart2}

parseLineSegment :: [Int] -> LineSegment
parseLineSegment [x1, y1, x2, y2] = LineSegment{start=Point{x=x1,y=y1}, end=Point{x=x2,y=y2}}
parseLineSegment _ = error "unexpected input"

data Point = Point{x :: Int, y :: Int} deriving (Eq, Ord)
data LineSegment = LineSegment{start :: Point, end :: Point}

solvePart1 :: [LineSegment] -> Int
solvePart1 = length . findDuplicates . concatMap enumeratePoints . filter isGridAligned

solvePart2 :: [LineSegment] -> Int
solvePart2 = length . findDuplicates . concatMap enumeratePoints

enumeratePoints :: LineSegment -> [Point]
enumeratePoints (LineSegment{start=Point{x=x1,y=y1}, end=Point{x=x2,y=y2}}) =
  let dx = signum (x2 - x1)
      dy = signum (y2 - y1)
  in zipWith (\x y -> Point{x, y}) [x1,x1+dx..x2] [y1,y1+dy..y2]

isGridAligned :: LineSegment -> Bool
isGridAligned LineSegment{start=Point{x=x1,y=y1}, end=Point{x=x2,y=y2}} = x1 == x2 || y1 == y2

findDuplicates :: (Ord a) => [a] -> [a]
findDuplicates = map fst . filter ((>1) . snd) . MultiSet.toOccurList . MultiSet.fromList

  






