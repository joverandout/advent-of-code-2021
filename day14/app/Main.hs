module Main where

import Data.Map as Map
import Data.List.Split
import Lib

type Rules = Map.Map String String

main :: IO ()
main = someFunc

sequence = "CKKOHNSBPCPCHVNKHFFK"
rules = ["KO -> C","SO -> S","BF -> V","VN -> B","OV -> K","VH -> O","KV -> N","KB -> F","NB -> C","HS -> K","PF -> B","HB -> N","OC -> H","FS -> F","VV -> S","KF -> C","FN -> F","KP -> S","HO -> N","NH -> K","OO -> S","FB -> C","BP -> F","CH -> N","SN -> O","KN -> B","CV -> O","CC -> B","VB -> C","PH -> V","CO -> K","KS -> K","BK -> N","FH -> S","PV -> H","CB -> P","FO -> F","BB -> K","OB -> C","HH -> F","ON -> O","FK -> B","NF -> F","SV -> F","CP -> H","SS -> B","OP -> H","NS -> O","HK -> N","BC -> P","NV -> V","VS -> F","PC -> V","CS -> F","NP -> V","PS -> F","VC -> F","KK -> S","PO -> P","HF -> H","KC -> P","SF -> N","BV -> N","FF -> V","FV -> V","BO -> N","OS -> C","OF -> H","CN -> S","NO -> O","NC -> B","VK -> C","HN -> B","PK -> N","SK -> S","HV -> F","BH -> B","OK -> S","VO -> B","BS -> H","PP -> N","SC -> K","BN -> P","FC -> S","SB -> B","SH -> H","NN -> V","NK -> N","VF -> H","CF -> F","PB -> C","SP -> P","KH -> C","VP -> N","CK -> H","HP -> P","FP -> B","HC -> O","PN -> F","OH -> H"]

testSequence = "NNCB"
testRules = ["CH -> B","HH -> N","CB -> H","NH -> C","HB -> C","HC -> B","HN -> C","NN -> C","BH -> H","NC -> B","NB -> B","BN -> B","BB -> N","BC -> B","CC -> N","CN -> C"]

rulesToMap :: [String] -> Map String String
rulesToMap stringys = Map.fromList (splitAllStrings stringys)

splitAllStrings :: [String] -> [(String, String)]
splitAllStrings [] = []
splitAllStrings [x] = [splitString x]
splitAllStrings (x:xs) = splitString x : splitAllStrings xs

splitString :: String -> (String, String)
splitString = arrayToTuple . splitOn " -> "

arrayToTuple :: [String] -> (String, String)
arrayToTuple [a,b] = (a,b)
arrayToTuple _ = error "Should only get array of length 2"

replace :: String -> Map String String -> String
replace [] mapp = ""
replace [x] mapp = [x]
replace (x:y:xs) mapp = case Map.lookup [x, y] mapp of
    Nothing -> x : replace (y:xs) mapp
    Just r -> (x : r) ++ (replace (y : xs) mapp)

part1Steps :: Int -> String -> Map String String -> String
part1Steps 10 stringy mapp = stringy
part1Steps x stringy mapp  = part1Steps (x+1) (replace stringy mapp) mapp