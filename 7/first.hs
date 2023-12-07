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
-- let result2 = discreteDurationWidth $ winningHoldDurationRadius (51926890, 222203111261225)
-- print result2

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

cardOrdering :: [Char]
cardOrdering = reverse "AKQJT98765432"

compareEqualType :: String -> String -> Ordering
compareEqualType [] [] = EQ
compareEqualType (x : xs) (y : ys)
    | x == y = compareEqualType xs ys
    | otherwise = compare (elemIndex x cardOrdering) (elemIndex y cardOrdering)

compareHand :: String -> String -> Ordering
compareHand a b
    | findHandType a == findHandType b = compareEqualType a b
    | otherwise = compare (findHandType a) (findHandType b)

toInt :: String -> Int
toInt x = read x :: Int

parseLine :: String -> (String, Int)
parseLine = ((\(a : b : _) -> (a, toInt b)) . words)

-- solution1 :: [String] -> Int
solution1 ls = foldl' (+) 0 $ zipWith (\a b -> a * snd b) [1 ..] $ sortBy (\a b -> compareHand (fst a) (fst b)) (map parseLine ls)
