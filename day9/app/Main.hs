module Main where

import Data.List.Split (splitOn)
import Control.Arrow

type Input = [Integer]

main :: IO ()
main = readFile "input.txt" >>= print . {-(part1 &&& part2) .-} prepare

prepare :: String -> Input
prepare = map read . lines

longIntToArray :: Integral x => x -> [x]
longIntToArray 0 = []
longIntToArray x = longIntToArray (x `div` 10) ++ [x `mod` 10]

arrayToLows :: [Int] -> Int -> [Int]
arrayToLows [x] y
            | x < y = [x]
            | otherwise = []
arrayToLows (x:xs) y
            | x < y && x < head xs =  x : arrayToLows xs x
            | otherwise = arrayToLows xs x

-- part1 :: Input -> Int
-- part1 input = const()

-- part2 :: Input -> Int
-- part2 input = const()