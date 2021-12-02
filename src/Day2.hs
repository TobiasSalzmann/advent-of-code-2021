{-# LANGUAGE NamedFieldPuns #-}

module Day2 (config) where
  
import Lib
         
data Command = Forward Int | Up Int | Down Int

data Pos = Pos {x :: Int, y:: Int, aim :: Int} deriving Show

         
config :: Config [Command] Int Int
config = Config {title = "Day 2"
                ,input = Lib.parseLinesWith parseCommand "resources/day2.txt"
                ,part1 = eval . solvePart1
                ,part2 = eval . solvePart2}


parseCommand :: String -> Command
parseCommand s = case words s of
  ["forward", val] -> Forward (read val)
  ["up", val] -> Up (read val)
  ["down", val] -> Down (read val)
  _ -> error "parse error"           
                
eval :: Pos -> Int
eval (Pos {x, y}) = x * y


solvePart1 :: [Command] -> Pos
solvePart1 = foldl step1 (Pos{x=0,y=0,aim=0})

step1 :: Pos -> Command -> Pos
step1 pos (Forward dx) = pos {x=(x pos + dx)}
step1 pos (Down dy) = pos {y=(y pos + dy)}
step1 pos (Up dy) = pos {y=(y pos - dy)}


solvePart2 :: [Command] -> Pos
solvePart2 = foldl step2 (Pos{x=0,y=0,aim=0})

step2 :: Pos -> Command -> Pos
step2 pos (Forward dx) = pos {x=(x pos + dx), y=(y pos + (aim pos * dx))}
step2 pos (Down dy) = pos {aim=(aim pos + dy)}
step2 pos (Up dy) = pos {aim=(aim pos - dy)}



