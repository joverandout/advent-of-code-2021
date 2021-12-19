module Main where

import Control.Arrow ((&&&))
import Data.List (foldl', sort)
import Data.Maybe (mapMaybe)

data Mismatch = Mismatch { _opener :: Char, _score :: Int } deriving Show
data ParseResult = Correct | Corrupted Int | Incomplete [Char] deriving Show

type Input = [String]


main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare

mismatch :: Char -> Maybe Mismatch
mismatch = flip lookup
  [ (')', Mismatch '(' 3)
  , (']', Mismatch '[' 57)
  , ('}', Mismatch '{' 1197)
  , ('>', Mismatch '<' 25137)
  ]

part2 :: Input -> Int
part2 = middle . sort . mapMaybe (calcScore . scoreC)
  where calcScore (Incomplete xs) = Just $ foldl' accum 0 xs
        calcScore _ = Nothing
        accum acc c = 5 * acc + score c
        score '(' = 1
        score '[' = 2
        score '{' = 3
        score '<' = 4
        middle [x] = x
        middle [] = error "even length"
        middle xs = middle (tail . init $ xs)

part1 :: Input -> Int
part1 = sum . mapMaybe (calcScore . scoreC)
  where calcScore (Corrupted n) = Just n
        calcScore _ = Nothing

prepare :: String -> Input
prepare = lines

scoreC :: [Char] -> ParseResult
scoreC = go []
  where go [] [] = Correct
        go s [] = Incomplete s
        go stack (i:input) = case mismatch i of
          Nothing -> go (i:stack) input
          Just (Mismatch expected score) -> case stack of
            [] -> error "no final bracket at all"
            s:more | expected == s -> go more input
                   | otherwise -> Corrupted score