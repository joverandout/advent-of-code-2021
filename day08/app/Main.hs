module Main where

import Data.Bool (bool)
import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Control.Arrow

type SignalCombination = [Bool]
type Entry = ([SignalCombination], [SignalCombination])
type Input = [Entry]
type Mapping = [Int]

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare

part1 :: Input -> Int
part1 = length . filter ((`elem` [2, 3, 4, 7]) . countWire) . transformInputA

part2 :: Input -> Int
part2 = sum . map getOutputNumber

splitInput :: String -> [String]
splitInput = splitOn "\n\n"

prepareSignalCombination :: String -> SignalCombination
prepareSignalCombination signal = map (`elem` signal) ['a' .. 'g']

countWire :: SignalCombination -> Int
countWire = sum . map (bool 0 1)

toDigit :: SignalCombination -> Maybe Int
toDigit [True, True, True, False, True, True, True] = Just 0
toDigit [False, False, True, False, False, True, False] = Just 1
toDigit [True, False, True, True, True, False, True] = Just 2
toDigit [True, False, True, True, False, True, True] = Just 3
toDigit [False, True, True, True, False, True, False] = Just 4
toDigit [True, True, False, True, False, True, True] = Just 5
toDigit [True, True, False, True, True, True, True] = Just 6
toDigit [True, False, True, False, False, True, False] = Just 7
toDigit [True, True, True, True, True, True, True] = Just 8
toDigit [True, True, True, True, False, True, True] = Just 9
toDigit _ = Nothing


applyMapping :: Mapping -> SignalCombination -> SignalCombination
applyMapping mapping combo = map (combo !!) mapping

isCorrectMapping :: Mapping -> [SignalCombination] -> Bool
isCorrectMapping mapping = all (isDigit . applyMapping mapping)

allPossibleMappings :: [Mapping]
allPossibleMappings = permutations [0 .. 6]

solveMapping :: [SignalCombination] -> Mapping
solveMapping combo = head $ filter (`isCorrectMapping` combo) allPossibleMappings

prepare :: String -> Input
prepare = map prepareEntry . lines

prepareEntry :: String -> Entry
prepareEntry input =
  let [left, right] = splitOn " | " input
   in (prepareCombinations left, prepareCombinations right)
  where
    prepareCombinations = map prepareSignalCombination . words

transformInputA :: Input -> [SignalCombination]
transformInputA = concatMap snd

getOutputNumber :: Entry -> Int
getOutputNumber (left, right) =
  let mapping = solveMapping left
   in decimalize $ map (fromJust . toDigit . applyMapping mapping) right

decimalize :: [Int] -> Int
decimalize = foldl (\acc x -> acc * 10 + x) 0

isDigit :: SignalCombination -> Bool
isDigit combo = case toDigit combo of Just _ -> True; Nothing -> False
