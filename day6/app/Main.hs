module Main where

import Lib
import Data.List.Split (splitOn)

type Input = [Fish]
type Fish = Int

main :: IO ()
main = readFile "input.txt" >>= print . part1 . prepare

prepare :: String -> Input
prepare = map read . splitOn ","

updateFish :: Fish -> [Int]
updateFish 0 = [6,8]
updateFish x = [x-1]

updateFishes :: [Fish] -> [Fish]
updateFishes [x] = updateFish x
updateFishes (x:xs) = updateFish x ++ updateFishes xs

eightyUpdates :: Int -> [Fish] -> [Fish]
eightyUpdates 80 fish = fish
eightyUpdates x fish = eightyUpdates (x+1) (updateFishes fish)

part1 :: Input -> Int
part1 fishes = length (eightyUpdates 0 fishes)