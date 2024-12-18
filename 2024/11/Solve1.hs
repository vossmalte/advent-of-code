module Solve1 where

import Text.Read (read)

input = [4329, 385, 0, 1444386, 600463, 19, 1, 56615]

parse s = read s :: Integer

split :: Integer -> [Integer]
split stone = map (parse . \part -> part (show stone)) [take middle, drop middle]
 where
  middle = length (show stone) `div` 2

blink :: [Integer] -> [Integer]
blink = foldr transformStone []
 where
  transformStone stone acc
    | stone == 0 = 1 : acc
    | length (show stone) `mod` 2 == 1 = stone * 2024 : acc
    | otherwise = split stone ++ acc

solve1 numBlinks = length . last . take (numBlinks + 1) . iterate blink

-- main :: IO
main = do
  print $ solve1 25 input
