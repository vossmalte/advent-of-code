-- import System.IO
import Test.QuickCheck
import Data.List.Extra

main :: IO ()
main = do
  contents <- readFile "input-first.txt"
  -- putStrLn contents
  let ls = filter (not . (==) []) (lines contents)
  let onlyDigits = map (filter isDigit) ls
  let onlyFirstAndLast = map firstAndLast onlyDigits
  let asInt = map toInt onlyFirstAndLast
  let result = sum asInt
  print result

main2 :: IO ()
main2 = do
  contents <- readFile "input.txt"
  let result = secondSolution $ lines contents
  print result

secondSolution :: [String] -> Int
secondSolution lines = sum $ map ( toInt . firstAndLast . (filter isDigit) . replaceAllWords ) lines
  
toInt :: String -> Int
toInt x = read x :: Int

prop_toInt :: Int -> Bool
prop_toInt x = x ==  (toInt $ show x)

numberWords = ["one","two","three","four","five","six","seven","eight","nine"]

digits = map (head . show) [1..9]

replaceTuple :: (String,Char) -> (String -> String)
replaceTuple (word,digit) = replace word (word++[digit]++word)

replacers :: [String -> String]
replacers = map replaceTuple (zip numberWords digits)

replaceAllWords :: String -> String
-- replaceAllWords = sequence replacers -- does not work :(
replaceAllWords = foldl (.) id replacers

isDigit = flip elem digits

firstAndLast :: [a] -> [a]
firstAndLast [] = []
firstAndLast xs = (head xs):(last xs):[]

prop_firstAndLast :: [a] -> Bool
prop_firstAndLast [] = 0== length  (firstAndLast [])
prop_firstAndLast xs = 2== (length  (firstAndLast xs))
