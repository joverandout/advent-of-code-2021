module Main where

import Lib
import Data.List.Split (splitOn)

type Input = [Fish]
type Fish = Int

main :: IO ()
main = readFile "input.txt" >>= print . part1 . prepare

prepare :: String -> Input
prepare = map read . splitOn ","

part1 :: Input -> Int
part1 fishes = length (ageSchoolNTimes 80 fishes)

part2 :: Input -> Int
part2 fishes = length (ageSchoolNTimes 256 fishes)

updateFish :: Fish -> [Int]
updateFish 0 = [6,8]
updateFish x = [x-1]

updateFishes :: [Fish] -> [Fish]
updateFishes [x] = updateFish x
updateFishes (x:xs) = updateFish x ++ updateFishes xs

ageSchoolNTimes :: Int -> [Fish] -> [Fish]
ageSchoolNTimes n school = iterate updateFishes school !! n