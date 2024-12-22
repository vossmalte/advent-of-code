module Main where

import Data.Bits
import Input

-- 16777216
infinity :: Int
infinity = shift 1 24

prune = (.&.) (infinity - 1)

step n x = prune $ xor x $ shift x n

next x = foldl' (flip step) x [6,(-5),11]

lastSecret start = iterate next start !! 2000

solve1 = sum . map lastSecret

inputTest = [ 1, 10, 100, 2024 ]

main :: IO ()
main = do
  putStrLn $ "Test 1: " ++ (show $ solve1 inputTest == 37327623)
  putStrLn $ "Part 1: " ++ (show $ solve1 input)
