module Main where

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Control.Arrow ((&&&))

type Point = (Int, Int)
data Axis = XAxis | YAxis deriving (Show, Eq)
type Fold = (Axis, Int)
type Input = (Set.Set Point, [Fold])

part1 :: Input -> Int
part1 (dots, firstFold : _) = Set.size $ Set.map (fold firstFold) dots

part2 :: Input -> String
part2 (dots, folds) =
  let afterAllFolds = foldl foldAll dots folds
   in "\n" ++ display afterAllFolds

main :: IO()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare

prepareFold :: String -> Fold
prepareFold = (\(dir : _ : rest) -> (prepareDir dir, read rest)) . drop 11

prepareDir :: Char -> Axis
prepareDir 'x' = XAxis
prepareDir 'y' = YAxis

prepareDot :: String -> Point
prepareDot dot =
  let [x, y] = map read $ splitOn "," dot
   in (x, y)


prepare :: String -> Input
prepare input =
  let [dots, folds] = splitOn "\n\n" input
      dots' = map prepareDot $ lines dots
      folds' = map prepareFold $ lines folds
   in (Set.fromList dots', folds')


fold :: Fold -> Point -> Point
fold (XAxis, k) (x, y) = (k - abs (x - k), y)
fold (YAxis, k) (x, y) = (x, k - abs (y - k))


foldAll :: Set.Set Point -> Fold -> Set.Set Point
foldAll dots f = Set.map (fold f) dots

display :: Set.Set Point -> String
display points =
  let
      xmax = maximum $ map fst $ Set.toList points
      ymax = maximum $ map snd $ Set.toList points
   in unlines [[if (x, y) `Set.member` points then '#' else '.' | x <- [0 .. xmax]] | y <- [0 .. ymax]]
