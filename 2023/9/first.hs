import Data.List.Extra

main :: IO ()
main = do
    contents1 <- readFile "9/input-first.txt"
    let ls1 = lines contents1
    let result1 = solution1 ls1
    print result1
    let result2 = solution2 ls1
    print result2

    contents <- readFile "9/input.txt"
    let ls = lines contents
    print $ solution1 ls
    print $ solution2 ls

toInt :: String -> Int
toInt x = read x :: Int

parseLine :: ([Int] -> [Int]) -> String -> [Int]
parseLine rev = rev . map toInt . words

difference :: [Int] -> [Int]
difference xs = zipWith (-) xs $ tail xs

buildTree :: [Int] -> [[Int]]
buildTree xs = takeWhile (not . all (0 ==)) $ iterate difference xs

calculateNextTop :: (Int -> Int -> Int) -> [[Int]] -> Int
calculateNextTop pm = pm 0 . foldl' pm 0 . map head

solution1 :: [String] -> Int
solution1 ls = sum $ map (calculateNextTop (+) . buildTree . parseLine reverse) ls

solution2 :: [String] -> Int
solution2 ls = sum $ map (calculateNextTop (-) . reverse . buildTree . parseLine id) ls
