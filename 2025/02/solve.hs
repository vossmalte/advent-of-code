isRepeating x = start == end
  where
    str = show x
    (start, end) = splitAt (length str `div` 2) str

solvePart1 :: [[Integer]] -> Integer
solvePart1 = sum . concatMap (filter isRepeating)

repeatPrefix str len = concat $ replicate (length str `div` len) (take len str)

isMultiRepeating x = any ((==str) . repeatPrefix str) [1..length str - 1]
  where
    str = show x

solvePart2 :: [[Integer]] -> Integer
solvePart2 = sum . concatMap (filter isMultiRepeating)
