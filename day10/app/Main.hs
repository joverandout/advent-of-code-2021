module Main where

import Control.Arrow ((&&&))
import Data.List (foldl', sort)
import Data.Maybe (mapMaybe)

data Mismatch = Mismatch { _opener :: Char, _score :: Int } deriving Show
data ParseResult = Correct | Corrupted Int | Incomplete [Char] deriving Show

type Input = [String]


main :: IO ()
main = readFile "input.txt" >>= print . part1 . prepare

mismatch :: Char -> Maybe Mismatch
mismatch = flip lookup
  [ (')', Mismatch '(' 3)
  , (']', Mismatch '[' 57)
  , ('}', Mismatch '{' 1197)
  , ('>', Mismatch '<' 25137)
  ]


part1 :: Input -> Int
part1 = sum . mapMaybe (getScore . score)
  where getScore (Corrupted n) = Just n
        getScore _ = Nothing

prepare :: String -> Input
prepare = lines

score :: [Char] -> ParseResult
score = go []
  where go [] [] = Correct
        go s [] = Incomplete s
        go stack (i:input) = case mismatch i of
          Nothing -> go (i:stack) input
          Just (Mismatch expected score) -> case stack of
            [] -> error "no final bracket at all"
            s:more | expected == s -> go more input
                   | otherwise -> Corrupted score