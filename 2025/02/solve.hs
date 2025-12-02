isRepeating x = start == end
  where
    str = show x
    (start, end) = splitAt (length str `div` 2) str

solve :: [[Integer]] -> Integer
solve = sum . concatMap (filter isRepeating)
