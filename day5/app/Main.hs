module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Lib

type Point = (Int, Int)
type Line = (Point, Point)
type LineCoverage = Map.Map Point Int

type Input = [Line]

vertical :: Line -> Bool
vertical ((x1, y1), (x2, y2)) = x1 == x2

horizontal :: Line -> Bool
horizontal ((x1, y1), (x2, y2)) =  y1 == y2

prepare :: String -> Input
prepare = map parseLine . lines

parseLine :: String -> Line
parseLine = first . map parsePoint . splitOn " -> "

parsePoint :: String -> Point
parsePoint = first . map read . splitOn ","

first :: [b] -> (b,b)
first (a:b:xs) = (a,b)
first [a] = error "First can only work on a list of size 2 or more"

getLineCoverage :: Line -> [Point]
getLineCoverage line@((x1, y1), (x2, y2))
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

part1 :: Input -> Int
part1 input = 
    let coverage = foldl addLineToMap Map.empty input
    in length $ filter (\(_, v) -> v > 1) $ Map.toList coverage

addPointToMap :: LineCoverage -> Point -> LineCoverage
addPointToMap coverage point = Map.insertWith (+) point 1 coverage

addLineToMap :: LineCoverage -> Line -> LineCoverage
addLineToMap coverage = foldl addPointToMap coverage . getLineCoverage

main :: IO ()
main = readFile "input.txt" >>= print . part1 . prepare