module Main where

import Data.List

isKey :: [String] -> Bool
isKey ("....." : xs) = True
isKey _ = False

isLock :: [String] -> Bool
isLock ("#####" : xs) = True
isLock _ = False

isSeparator :: String -> Bool
isSeparator "" = True
isSeparator _ = False

splitInput :: [String] -> [[String]]
splitInput [] = []
splitInput [""] = []
splitInput xs =
  let (a, b) = break isSeparator xs
   in a : splitInput (drop 1 b)

toNums :: [String] -> [Int]
toNums = map (negate . (1 -) . length . takeWhile (== '#')) . transpose

match :: [Int] -> [Int] -> Bool
match a b = 6 > maximum (zipWith (+) a b)

countMatches :: [[Int]] -> [Int] -> Int
countMatches keys lock = length $ filter (match lock) keys

count :: [[Int]] -> [[Int]] -> Int
count keys = sum . map (countMatches keys)

process ls = count (map toNums locks) (map toNums keys)
 where
  keys = map reverse rawKeys
  (locks, rawKeys) = partition isLock keysAndLocks
  keysAndLocks = splitInput ls

main :: IO ()
main = do
  fileTest <- readFile "inputTest.txt"
  putStr "Test: 3 == "
  print $ process $ lines fileTest
  putStr "Solvution 1 : "
  input <- readFile "input.txt"
  print $ process $ lines input
