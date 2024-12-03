maxstep, minstep :: Int
maxstep = 3
minstep = 1

step :: [Int] -> [Int]
step xs = zipWith (-) xs (tail xs)

isSortedByStep :: [Int] -> Bool
isSortedByStep xs = all (>0) xs || (all (<0) xs)

isLegitSteps :: [Int] -> Bool
isLegitSteps xs = maxstep >= maximum zs && minstep <= minimum zs
  where zs = map abs xs

valid :: [Int] -> Bool
valid xs = isLegitSteps (step xs) && isSortedByStep (step xs)

solve :: [[Int]] -> Int
solve = length . filter valid

generator :: [Int] -> [[Int]]
generator xs = flip map [0..length xs] $ \i -> dropOut i
  where dropOut i = take i xs ++ drop (i+1) xs

solve2 :: [[Int]] -> Int
solve2 = length . filter (any valid) . map generator
