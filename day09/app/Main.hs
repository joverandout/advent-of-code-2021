module Main where

import Data.List.Split (splitOn)
import Control.Arrow

zeros =  [99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99]
arrayTest = [[2,1,9,9,9,4,3,2,1,0,2,1,9,9,9,4,3,2,1,0,2,1,9,9,9,4,3,2,1,0,2,1,9,9,9,4,3,2,1,0,2,1,9,9,9,4,3,2,1,0,2,1,9,9,9,4,3,2,1,0,2,1,9,9,9,4,3,2,1,0,2,1,9,9,9,4,3,2,1,0], [3,9,8,7,8,9,4,9,2,1,3,9,8,7,8,9,4,9,2,1,3,9,8,7,8,9,4,9,2,1,3,9,8,7,8,9,4,9,2,1,3,9,8,7,8,9,4,9,2,1,3,9,8,7,8,9,4,9,2,1,3,9,8,7,8,9,4,9,2,1,3,9,8,7,8,9,4,9,2,1], [9,8,5,6,7,8,9,8,9,2,9,8,5,6,7,8,9,8,9,2,9,8,5,6,7,8,9,8,9,2,9,8,5,6,7,8,9,8,9,2,9,8,5,6,7,8,9,8,9,2,9,8,5,6,7,8,9,8,9,2,9,8,5,6,7,8,9,8,9,2,9,8,5,6,7,8,9,8,9,2], [8,7,6,7,8,9,6,7,8,9,9,8,5,6,7,8,9,8,9,2,8,7,6,7,8,9,6,7,8,9,9,8,5,6,7,8,9,8,9,2,8,7,6,7,8,9,6,7,8,9,9,8,5,6,7,8,9,8,9,2,8,7,6,7,8,9,6,7,8,9,9,8,5,6,7,8,9,8,9,2], [9,8,9,9,9,6,5,6,7,8,9,8,5,6,7,8,9,8,9,2,9,8,9,9,9,6,5,6,7,8,9,8,5,6,7,8,9,8,9,2,9,8,9,9,9,6,5,6,7,8,9,8,5,6,7,8,9,8,9,2,9,8,9,9,9,6,5,6,7,8,9,8,5,6,7,8,9,8,9,2]]
zerosTest = [9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]

main :: IO ()
main = readFile "input.txt" >>= print . part1 . prepare3. prepare

sumVals :: [Integer] -> Integer
sumVals xs = sum (map (+1) xs)

prepare :: String -> [Integer]
prepare = map read . lines

prepare4 :: [[Integer]] -> [Int]
prepare4 [x] = [length x]
prepare4 (x:xs) = length x : prepare4 xs

prepare3 :: [Integer] -> [[Integer]]
prepare3 [x] = [prepare2 x]
prepare3 (x:xs) = prepare2 x : prepare3 xs

prepare2 :: Integer -> [Integer]
prepare2 0 = []
prepare2 x = prepare2 (x `div` 10) ++ [x `mod` 10]

longIntToArray :: Integral x => x -> [x]
longIntToArray 0 = []
longIntToArray x = longIntToArray (x `div` 10) ++ [x `mod` 10]

runArraysToLows :: [[Integer]] -> [Integer] -> [Integer]
runArraysToLows [] lastRun = []
runArraysToLows [x] lastRun = arrayToLows lastRun x zerosTest 9999999
runArraysToLows (x:xs) lastRun = arrayToLows lastRun x (head xs) 9999999 ++ runArraysToLows xs x

arrayToLows :: [Integer] -> [Integer] -> [Integer] -> Integer -> [Integer]
arrayToLows [z] [x] [w] y
            | x < y && x < z && x < w = [x]
            | otherwise = []
arrayToLows (z:zs) (x:xs) (w:ws) y
            | x < y && x < head xs && x < z && x < w =  x : arrayToLows zs xs ws x
            | otherwise = arrayToLows zs xs ws x


part1 :: [[Integer]] -> Integer
part1 input = sumVals (runArraysToLows input zeros)

-- part2 :: Input -> Int
-- part2 input = const()