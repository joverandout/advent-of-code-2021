module Main where

import Data.List.Split (splitOn)
import Control.Arrow

type Input = [Int]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare

prepare :: String -> Input
prepare = map read . splitOn ","

getDistance :: Int -> [Int] -> [Int]
getDistance target = map (abs . (target -))

getFuelCostA :: Int -> [Int] -> Int
getFuelCostA target = sum . getDistance target

fuelCostB :: Int -> Int
fuelCostB n = n * (n + 1) `div` 2

getFuelCostB :: Int -> [Int] -> Int
getFuelCostB target = sum . map fuelCostB . getDistance target

part1 :: Input -> Int
part1 input = minimum $ map (`getFuelCostA` input) [0 .. 1400]

part2 :: Input -> Int
part2 input = minimum $ map (`getFuelCostB` input) [0 .. 1400]



