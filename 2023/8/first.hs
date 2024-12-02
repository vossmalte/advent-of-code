import Data.HashMap.Strict hiding (filter, foldl', map)
import Data.List.Extra hiding (lookup)
import Data.Maybe
import Prelude hiding (lookup)

main :: IO ()
main = do
    contents1 <- readFile "8/input-first.txt"
    let ls1 = lines contents1
    let result1 = solution1 ls1
    print result1

    contents <- readFile "8/input.txt"
    let ls = lines contents
    -- print $ solution1 ls

    -- part 2
    contents2 <- readFile "8/input-second.txt"
    let ls2 = lines contents2
    let result2 = solution2 ls2
    print result2

    print $ solution2 ls

letters :: [Char]
letters = ['A' .. 'Z'] ++ "123456789"

directionToLocationPicker :: Char -> (a, a) -> a
directionToLocationPicker 'L' = fst
directionToLocationPicker _ = snd

nextLocation :: HashMap String (String, String) -> String -> Char -> String
nextLocation hmap location direction = directionToLocationPicker direction (fromMaybe ("", "") $ lookup location hmap)

parseLine :: String -> (HashMap String (String, String) -> HashMap String (String, String))
parseLine line = Data.HashMap.Strict.insert key (l, r)
  where
    (key, l, r) = (ws !! 0, ws !! 1, ws !! 2)
      where
        ws = filter (not . (==) "") $ map (filter $ flip elem letters) $ words line

createHashMap :: [String] -> HashMap String (String, String)
createHashMap = foldl' (flip parseLine) empty

parseAllNodes :: [String] -> [String]
parseAllNodes lines = map (head . words) lines

thirdLetterIs :: (Eq a) => a -> [a] -> Bool
thirdLetterIs x [_, _, y] = x == y
thirdLetterIs _ _ = False

isStart = thirdLetterIs 'A'
isEnd = thirdLetterIs 'Z'

locations :: HashMap String (String, String) -> String -> [String]
locations hmap directions = "AAA" : zipWith (nextLocation hmap) (locations hmap directions) (cycle directions)
locations2 startLocation hmap directions = startLocation : zipWith (nextLocation hmap) (locations hmap directions) (cycle directions)

ghostLocations :: HashMap String (String, String) -> String -> [String] -> [[String]]
ghostLocations hmap directions startLocations = startLocations : zipWith (\ls direction -> map (\l -> nextLocation hmap l direction) ls) (ghostLocations hmap directions startLocations) (cycle directions)

solution1 :: [String] -> Int
solution1 ls = length $ takeWhile (not . (==) "ZZZ") $ locations (createHashMap $ drop 2 ls) $ head ls

solution2 ls = map (\startLocation -> length $ takeWhile (not . isEnd) $ locations2 startLocation (createHashMap $ drop 2 ls) $ head ls) (filter isStart $ parseAllNodes $ drop 2 ls)
-- solution2 ls = takeWhile (not . all isEnd) $ ghostLocations (createHashMap $ drop 2 ls) (head ls) (filter isStart $ parseAllNodes $ drop 2 ls)

-- solution2 :: [String] -> Int
-- solution2 ls = foldl' (+) 0 $ zipWith (\a b -> a * snd b) [1 ..] $ sortBy (\a b -> compareHand2 (fst a) (fst b)) (map parseLine ls)
