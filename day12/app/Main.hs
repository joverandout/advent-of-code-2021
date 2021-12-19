module Main where

import Lib
import Data.Char (isUpper)
import Data.Function ((&))
import Data.Graph (path)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow ((&&&))

type Node = String
type Graph = (Set.Set Node, Map.Map Node [Node])

prepare :: String -> Graph
prepare = foldl buildGraph emptyGraph . map prepareLine . lines

prepareLine :: String -> (String, String)
prepareLine line = let [a, b] = splitOn "-" line in (a, b)

buildGraph :: Graph -> (String, String) -> Graph
buildGraph (nodes, edges) (from, to) =
  ( nodes & Set.insert from & Set.insert to,
    edges & Map.insertWith (++) from [to] & Map.insertWith (++) to [from]
  )

emptyGraph :: Graph
emptyGraph = (Set.empty, Map.empty)

nodeLarge :: Node -> Bool
nodeLarge = isUpper . head

part1 :: [Node] -> Graph -> Int
part1 ("end" : _) _ = 1
part1 path graph@(_, edges) =
  let potentialNextSteps = edges & Map.findWithDefault [] (head path)
      nextSteps = filter (\cave -> nodeLarge cave || cave `notElem` path) potentialNextSteps
   in sum $ map (\cave -> part1 (cave : path) graph) nextSteps

main :: IO()
main = readFile "input.txt" >>= print . part1 ["start"] . prepare