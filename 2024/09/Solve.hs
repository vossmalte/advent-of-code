module Solve where

import Data.List

free :: Int
free = -1

decode :: Int -> [Int] -> [Int]
decode _ [] = []
decode id [x] = replicate x id
decode id (x:y:xs) = replicate x id ++ replicate y free ++ decode (id+1) xs

compress :: [Int] -> [Int]
compress [] = []
compress [-1] = []
compress [x] = [x]
compress (-1:xs) = last xs :  compress (dropWhileEnd (-1==) $ init xs)
compress (x:xs) = x : compress xs

solve = sum . zipWith (*) [0..] . compress . decode 0
