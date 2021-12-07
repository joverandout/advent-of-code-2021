module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Lib
import Control.Arrow


type Point = (Int, Int)
type Line = (Point, Point)
type LineCoverage = Map.Map Point Int

type Input = [Line]

vertical :: Line -> Bool
vertical ((x1, y1), (x2, y2)) = x1 == x2

horizontal :: Line -> Bool
horizontal ((x1, y1), (x2, y2)) =  y1 == y2

diagonal :: Line -> Bool
diagonal ((x1, y1), (x2, y2)) = abs (x1-x2) == abs (y1-y2)

prepare :: String -> Input
prepare = map parseLine . lines

parseLine :: String -> Line
parseLine = firstVal . map parsePoint . splitOn " -> "

parsePoint :: String -> Point
parsePoint = firstVal . map read . splitOn ","

firstVal :: [b] -> (b,b)
firstVal (a:b:xs) = (a,b)
firstVal [a] = error "firstVal can only work on a list of size 2 or more"

getLineCoverage2 :: Line -> [Point]
getLineCoverage2 line@((x1, y1), (x2, y2))
                | vertical line = getCoverageOfVerticalLine line (y1 > y2) (y1 == y2)
                | horizontal line = getCoverageOfHorizontalLine line (x1 > x2) (x1 == x2)
                | diagonal line = getCoverageOfDiagonalLine line
                | otherwise = []

getLineCoverage1 :: Line -> [Point]
getLineCoverage1 line@((x1, y1), (x2, y2))
                | vertical line = getCoverageOfVerticalLine line (y1 > y2) (y1 == y2)
                | horizontal line = getCoverageOfHorizontalLine line (x1 > x2) (x1 == x2)
                | otherwise = []

getCoverageOfVerticalLine :: Line -> Bool -> Bool -> [Point]
getCoverageOfVerticalLine ((x1, y1), (x2, y2)) _ True = [(x1, y2)]
getCoverageOfVerticalLine ((x1, y1), (x2, y2)) True False  = (x1, y1) : getCoverageOfVerticalLine ((x1, y1 - 1), (x2, y2)) True  ((y1 - 1) == y2)
getCoverageOfVerticalLine ((x1, y1), (x2, y2)) False False = (x1, y1) : getCoverageOfVerticalLine ((x1, y1 + 1), (x2, y2)) False ((y1 + 1) == y2)

getCoverageOfHorizontalLine :: Line -> Bool -> Bool -> [Point]
getCoverageOfHorizontalLine ((x1, y1), (x2, y2)) _ True = [(x1, y2)]
getCoverageOfHorizontalLine ((x1, y1), (x2, y2)) True False  = (x1, y1) : getCoverageOfHorizontalLine ((x1 - 1, y1), (x2, y2)) True  ((x1 - 1) == x2)
getCoverageOfHorizontalLine ((x1, y1), (x2, y2)) False False = (x1, y1) : getCoverageOfHorizontalLine ((x1 + 1, y1), (x2, y2)) False ((x1 + 1) == x2)

getCoverageOfDiagonalLine :: Line -> [Point]
getCoverageOfDiagonalLine ((x1, y1), (x2, y2)) =
    let xDirection = if x1 < x2 then 1 else -1
        yDirection = if y1 < y2 then 1 else -1
        difference = abs (x2-x1)
        x = x1
        y = y1
    in map (\d -> (x1 + (d * xDirection), y1 + (d * yDirection))) [0 .. difference]

part1 :: Input -> Int
part1 input = 
    let coverage = foldl addLineToMap1 Map.empty input
    in length $ filter (\(_, v) -> v > 1) $ Map.toList coverage

part2 :: Input -> Int
part2 input = 
    let coverage = foldl addLineToMap2 Map.empty input
    in length $ filter (\(_, v) -> v > 1) $ Map.toList coverage

addPointToMap :: LineCoverage -> Point -> LineCoverage
addPointToMap coverage point = Map.insertWith (+) point 1 coverage

addLineToMap1 :: LineCoverage -> Line -> LineCoverage
addLineToMap1 coverage = foldl addPointToMap coverage . getLineCoverage1


addLineToMap2 :: LineCoverage -> Line -> LineCoverage
addLineToMap2 coverage = foldl addPointToMap coverage . getLineCoverage2

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare