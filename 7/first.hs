import Data.List.Extra
import Data.Maybe
import Text.Regex.PCRE

main :: IO ()
main = do
    contents1 <- readFile "7/input-first.txt"
    let ls1 = lines contents1
    let result1 = solution1 ls1
    print result1

    contents <- readFile "7/input.txt"
    let ls = lines contents
    print $ solution1 ls

    -- part 2
    print $ solution2 ls

-- sorted from strongest to weakest
-- this might enable using find to find a first match
handTypeRegexs :: [[String]]
handTypeRegexs =
    [ ["(\\w)\\1{4}"] -- five of a kind
    , ["(\\w)\\1{3}"] -- four of a kind
    , ["(\\w)\\1{2}(\\w)\\2", "(\\w)\\1(\\w)\\2{2}"] -- full house
    , ["(\\w)\\1{2}"] -- three of a kind
    , ["(\\w)\\1.*(\\w)\\2"] -- two pairs
    , ["(\\w)\\1"] -- pair
    , ["\\w{5}"] -- high card
    ]

testR :: String -> String -> Bool
testR string regex = string =~ regex :: Bool

findHandType :: String -> Int
findHandType hand = length handTypeRegexs - fromMaybe (length handTypeRegexs) (findIndex (isJust . find (testR $ sort hand)) handTypeRegexs)

findBestHandType :: [Char] -> Int
findBestHandType hand = maximum $ map (findHandType . (\c -> replace "J" [c] hand)) cardOrdering

cardOrdering :: [Char]
cardOrdering = reverse "AKQJT98765432"

cardOrdering2 :: [Char]
cardOrdering2 = reverse "AKQT98765432J"

compareEqualType :: String -> String -> Ordering
compareEqualType [] [] = EQ
compareEqualType (x : xs) (y : ys)
    | x == y = compareEqualType xs ys
    | otherwise = compare (elemIndex x cardOrdering) (elemIndex y cardOrdering)
compareEqualType _ _ = EQ

compareEqualType2 :: String -> String -> Ordering
compareEqualType2 [] [] = EQ
compareEqualType2 (x : xs) (y : ys)
    | x == y = compareEqualType2 xs ys
    | otherwise = compare (elemIndex x cardOrdering2) (elemIndex y cardOrdering2)
compareEqualType2 _ _ = EQ

compareHand :: String -> String -> Ordering
compareHand a b
    | findHandType a == findHandType b = compareEqualType a b
    | otherwise = compare (findHandType a) (findHandType b)

compareHand2 :: String -> String -> Ordering
compareHand2 a b
    | findBestHandType a == findBestHandType b = compareEqualType2 a b
    | otherwise = compare (findBestHandType a) (findBestHandType b)

toInt :: String -> Int
toInt x = read x :: Int

parseLine :: String -> (String, Int)
parseLine = ((\(a : b : _) -> (a, toInt b)) . words)

solution1 :: [String] -> Int
solution1 ls = foldl' (+) 0 $ zipWith (\a b -> a * snd b) [1 ..] $ sortBy (\a b -> compareHand (fst a) (fst b)) (map parseLine ls)
solution2 :: [String] -> Int
solution2 ls = foldl' (+) 0 $ zipWith (\a b -> a * snd b) [1 ..] $ sortBy (\a b -> compareHand2 (fst a) (fst b)) (map parseLine ls)
