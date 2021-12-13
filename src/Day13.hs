{-# LANGUAGE NamedFieldPuns #-}

module Day13
  ( config
  ) where

import qualified Data.Set  as Set
import Data.Set (Set)
import           Lib
import qualified Data.List.Split as Split
import qualified Advent.OCR as OCR

data Instruction = FoldX Int | FoldY Int

data Sheet = Sheet {dots :: Set Point2D, instructions :: [Instruction]}
data MultilineString = MultilineString [String]

instance Show MultilineString where
  show (MultilineString xs) = unlines xs

config :: Config Sheet Int String
config =
  Config
    { title = "Day 13"
    , input = fmap parseSheet . parseLinesWith id $ "resources/day13.txt"
    , part1 = solvePart1
    , part2 = solvePart2
    }
    
parseSheet :: [String] -> Sheet
parseSheet lines =
  let (coords, "" : ins) = span (/="") lines
      dots = Set.fromList . map parsePoint $ coords
      instructions = map (parseInstruction . last . words) ins
  in Sheet { dots, instructions}
  
parseInstruction :: String -> Instruction
parseInstruction ('x':'=':x) = FoldX (read x)
parseInstruction ('y':'=':y) = FoldY (read y)

parsePoint :: String -> Point2D
parsePoint s =
  let [a, b] = Split.wordsBy (== ',') s
  in Point2D{xCoord= read a, yCoord = read b}
      

solvePart1 :: Sheet -> Int
solvePart1 (Sheet {dots, instructions=instruction:_}) = length . Set.map (applyInstruction instruction) $ dots

solvePart2 :: Sheet -> String
solvePart2 (Sheet {dots, instructions}) =
  let final = foldl (\acc i -> Set.map (applyInstruction i) acc) dots instructions
  in OCR.unsafeParseLetters . Set.map (\p -> (xCoord p, yCoord p)) $ final
  
visualizeDots :: Set Point2D -> MultilineString
visualizeDots dots =
  let ys = Set.map yCoord dots
  in MultilineString . map (visualizeLine dots) $ [Set.findMin ys..Set.findMax ys]
  
visualizeLine :: Set Point2D -> Int -> String
visualizeLine dots yCoord = 
  let xs = Set.map xCoord dots
  in [if Point2D{yCoord, xCoord} `elem` dots then '#' else ' ' | xCoord <- [Set.findMin xs..Set.findMax xs]]

applyInstruction :: Instruction -> Point2D -> Point2D
applyInstruction (FoldX x) p = p {xCoord = x - abs(xCoord p - x)}   
applyInstruction (FoldY y) p = p {yCoord = y - abs(yCoord p - y)}   


