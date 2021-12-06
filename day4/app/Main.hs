import Data.Char (isNumber)
import Data.List (groupBy, transpose)
import Data.Bifunctor (second)
import Control.Monad.State (State, evalState, gets, get, modify)
import Control.Arrow hiding (second)

type Board = [[(Bool, Int)]]

main :: IO ()
main =  readFile "input.txt" >>= print . (part1 &&& part2) . prepare . lines

part1 :: ([Int], [Board]) -> Int
part1 (i, bs) = uncurry (*) . second computeScore $ evalState (solvePart1 i) bs

part2 :: ([Int], [Board]) -> Int
part2 (i, bs) = uncurry (*) . second computeScore $ evalState (solvePart2 i) bs
 where
  solvePart2 :: [Int] -> State [Board] (Int, Board)
  solvePart2 []     = error "Somebody always wins"
  solvePart2 (x:xs) = do
      b <- get 
      if length b /= 1 then modify (filter (not . hasWon) . map (updateBoard x)) >> solvePart2 xs
                       else solvePart1 (x:xs)

prepare :: [String] -> ([Int], [Board])
prepare []     = ([], [])
prepare (l:ls) = (toInts l, toBoards ls)
 where
  toInts :: String -> [Int] 
  toInts =  map read . filter (/= ",") . groupBy (\x y -> isNumber x && isNumber y) 
  toBoards :: [String] -> [Board]
  toBoards [] = []
  toBoards xs = (map (zip (repeat False) . map read . words) . drop 1 . take 6) xs : toBoards (drop 6 xs)

solvePart1 :: [Int] -> State [Board] (Int, Board)
solvePart1 []     = error "Somebody always wins"
solvePart1 (x:xs) = do
    modify (map (updateBoard x))
    b <- gets (filter hasWon) 
    if null b then solvePart1 xs
              else return (x, head b)

hasWon :: Board -> Bool
hasWon b = let markings = map (map fst) b in any and markings || any and (transpose markings)

updateBoard :: Int -> Board -> Board
updateBoard x = map (map (\(b, y) -> if y == x then (True, y) else (b, y)))

computeScore :: Board -> Int
computeScore = sum . map snd . filter (not . fst). concat 
