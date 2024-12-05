-- prepare input with vim:
-- order:
-- %s/\(\d*\)|\(\d*\)/cmp \1 \2 = LT\rcmp \2 \1 = GT
-- lists:
-- %s/^\(.*\)$/[\1],/

module Check where
import Data.List

isSortedBy ::  (Integer -> Integer -> Ordering) -> [Integer] -> Bool
isSortedBy cmp xs = xs == (sortBy cmp xs)

middle :: [Integer] -> Integer
middle xs = xs !! (length xs `div` 2)

solve ::  (Integer -> Integer -> Ordering) -> [[Integer]] -> Integer
solve cmp  = sum . map middle . filter (isSortedBy cmp) 


solve2 ::  (Integer -> Integer -> Ordering) -> [[Integer]] -> Integer
solve2 cmp  = sum . map middle . map (sortBy cmp) . filter (not . isSortedBy cmp)
