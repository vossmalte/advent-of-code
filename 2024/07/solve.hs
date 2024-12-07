module Solve where
import Data.List
import Text.Read
import Combinatorics

m = (*)
a = (+)
c :: Integer -> Integer -> Integer
c a b = read $ show a ++ show b

operators :: Int -> [[Integer -> Integer -> Integer]]
-- task 1
-- operators = flip variateRep [m,a]
-- task 2 with concatenation
operators = flip variateRep [m,a,c]

calculate :: [Integer] -> [Integer -> Integer -> Integer] -> Integer 
calculate [x] _ = x
calculate (x:y:xs) (op:ops) = calculate ((op x y):xs) ops

checkOperators :: (Integer, [Integer]) -> [Integer -> Integer -> Integer] -> Bool
checkOperators (result, numbers) ops = result == calculate numbers ops

solveable :: (Integer, [Integer]) -> Bool
solveable (result, numbers) = any (checkOperators (result, numbers)) (operators $ length numbers - 1)

solve :: [(Integer, [Integer])] -> Integer
solve = sum . map fst . filter solveable
