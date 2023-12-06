import Data.List.Extra

main1 :: IO ()
main1 = do
  contents <- readFile "5/input.txt"
  let ls = lines contents
  let seeds = map toInt $ filter isDigitLine $ words $ head ls
  let splits = split (not . isDigitLine) $ tail ls
  let mappings = filter (not . isEmpty) $ flip map splits $ map (toTriple . (map toInt . words))
  let results = flip map seeds $ flip foldSeed mappings
  let result = minimum results
  print result

main :: IO ()
main = do
  contents <- readFile "5/input.txt"
  let ls = lines contents
  let seedInput = map toInt $ filter isDigitLine $ words $ head ls

  let splits = split (not . isDigitLine) $ tail ls
  let mappings = filter (not . isEmpty) $ flip map splits $ map (toTriple . (map toInt . words))

  let reverseMappings = map (map flipT) $ reverse mappings

  let checkExists = flip isInSeedList $ createSeedList seedInput
  let resultSeed = find checkExists $ flip map [1 ..] $ flip foldSeed reverseMappings
  let resultLocation = maybe 0 (flip foldSeed mappings) resultSeed
  print resultLocation

flipT :: (b, a, c) -> (a, b, c)
flipT (a, b, c) = (b, a, c)

createSeedList :: [Int] -> [(Int, Int)]
createSeedList [] = []
createSeedList [_] = []
createSeedList (start : len : xs) = (start, start + len - 1) : createSeedList xs

inRange :: (Int, Int) -> Int -> Bool
inRange (start, stop) x = x >= start && x <= stop

isInSeedList :: Int -> [(Int, Int)] -> Bool
isInSeedList x = foldl' (\a range -> a || inRange range x) False

iterateSeed :: [[(Int, Int, Int)]] -> Int -> [Int]
iterateSeed [] _ = []
iterateSeed mappings seed = seed : zipWith applyMapping (mappings) (iterateSeed mappings seed)

foldSeed :: Int -> [[(Int, Int, Int)]] -> Int
foldSeed = foldl' $ flip applyMapping

isDigitLine :: String -> Bool
isDigitLine [] = False
isDigitLine (x : _) = isDigit x

toInt :: String -> Int
toInt x = read x :: Int

toTriple :: [Int] -> (Int, Int, Int)
toTriple [a, b, c] = (a, b, c)
toTriple _ = (0, 0, 0)

isMappingApplicable :: (Int, Int, Int) -> Int -> Bool
isMappingApplicable (_destStart, sourceStart, len) source = source >= sourceStart && source < sourceStart + len

applyMapping :: [(Int, Int, Int)] -> Int -> Int
applyMapping m x = flip _applyMapping x $ filter (flip isMappingApplicable x) m

_applyMapping :: [(Int, Int, Int)] -> Int -> Int
_applyMapping [] x = x
_applyMapping ((destStart, sourceStart, _len) : _) source = source - sourceStart + destStart

digits :: [Char]
digits = map (head . show) [0 .. 9]

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

isDigit :: Char -> Bool
isDigit = flip elem digits
