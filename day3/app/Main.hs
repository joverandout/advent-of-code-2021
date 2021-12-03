module Main where

import Lib
import Control.Arrow 
import Data.List        

data Frequency = Frequency {ones, zeroes :: Int} deriving Show
data Bit = Zero | One deriving (Eq, Ord, Show, Enum)

type Input = [[Bit]]

main :: IO ()
main = do readFile "input.txt" >>= print . part1 . getFrequencies . transpose . prepare

part1 :: [Frequency] -> Int
part1 xs = fromDigits 2 (gamma xs) * fromDigits 2 (eps xs)

gamma :: [Frequency] -> [Int]
gamma [Frequency ones zeroes] = [fromEnum (ones > zeroes)]
gamma (x:xs) = gamma [x] ++ gamma xs

eps :: [Frequency] -> [Int]
eps [Frequency ones zeroes] = [fromEnum (ones < zeroes)]
eps (x:xs) = eps [x] ++ eps xs

getFrequencies :: [[Bit]] -> [Frequency]
getFrequencies [x] = [getFreq x (Frequency 0 0)]
getFrequencies (x:xs) = getFrequencies [x] ++ getFrequencies xs

getFreq :: [Bit] -> Frequency -> Frequency
getFreq [One] (Frequency ones zeroes) = Frequency (ones+1) zeroes
getFreq [Zero] (Frequency ones zeroes) = Frequency ones (zeroes+1)
getFreq (x:xs) f = getFreq xs (getFreq [x] f)

prepare :: String -> Input
prepare = map (map toBit) . lines
  where toBit '0' = Zero
        toBit '1' = One

fromDigits :: (Num a, Foldable f) => a -> f a -> a
fromDigits r = foldl ((+) . (r *)) 0