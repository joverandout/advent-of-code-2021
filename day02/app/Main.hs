module Main where

import Lib
import Control.Arrow         

data Direction = Up | Down | Forward deriving Show
data Instruction = Instruction Direction Int deriving Show
type Coordinates1 = (Int, Int)
type Coordinates2 = (Int, Int, Int)

main :: IO ()
main = do readFile "input.txt" >>= print . (part1 &&& part2) . prepare

part1 :: [Instruction] -> Int
part1 instructions = x * y where (x, y) = foldl updateCoords1 (0,0) instructions

part2 :: [Instruction] -> Int
part2 instructions = x * y where (x, y, z) = foldl updateCoords2 (0,0,0) instructions

updateCoords1 :: Coordinates1 -> Instruction -> Coordinates1
updateCoords1 (x,y) (Instruction m n) = case m of
    Forward -> (x+n, y)
    Up -> (x, y-n)
    Down -> (x, y+n)

prepare :: String -> [Instruction]
prepare = map (parse . words) . lines
    where parse [direction, val] = Instruction d (read val)
            where d = case direction of
                    "down" -> Down
                    "up" -> Up
                    "forward" -> Forward

updateCoords2 :: Coordinates2 -> Instruction -> Coordinates2
updateCoords2 (x, y, z) (Instruction m n) = case m of
    Forward -> (x+n, y+(z*n), z)
    Up -> (x, y, z-n)
    Down -> (x, y, z+n)