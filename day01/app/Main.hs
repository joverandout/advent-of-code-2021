module Main where

import Control.Arrow
import Lib

type Input = [Int]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare

prepare :: String -> Input
prepare = map read . lines

part1 :: Input -> Int
part1 (x:y:xs) = sub x y + part1 (y:xs)
part1 xs = 0

part2 :: Input -> Int
part2 (x:y:z:w:xs) = sub (x + y + z) (y + z + w) + part2 (y:z:w:xs)
part2 xs = 0

sub :: Int -> Int -> Int
sub x y = fromEnum (y > x)
