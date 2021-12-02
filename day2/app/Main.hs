module Main where

import Lib
import Control.Arrow         

data Direction = Up | Down | Forward deriving Show
data Motion = Motion Direction Int deriving Show
type Coordinates = (Int, Int)


main :: IO ()
main = do readFile "input.txt" >>= print . part1 . prepare

part1 :: [Motion] -> Int
part1 instructions = x * y where (x, y) = foldl updateCoords1 (0,0) instructions

updateCoords1 :: Coordinates -> Motion -> Coordinates
updateCoords1 (x,y) (Motion m n) = case m of
    Forward -> (x+n, y)
    Up -> (x, y-n)
    Down -> (x, y+n)

prepare :: String -> [Motion]
prepare = map (parse . words) . lines
    where parse [direction, val] = Motion d (read val)
            where d = case direction of
                    "down" -> Down
                    "up" -> Up
                    "forward" -> Forward