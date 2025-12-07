import Data.List (groupBy, transpose, unsnoc)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text qualified as T
import Text.Read (readMaybe)

main = do
  contents <- readFile "input.txt"
  print . solve1 $ contents
  print . solve2 $ contents

readInt a = readMaybe . T.unpack . T.strip . T.pack $ a :: Maybe Int

solve1 file = sum . zipWith f operators . transpose . map (map read) $ operands
  where
    (operands, operators) = fromMaybe ([], []) . unsnoc . map words . lines $ file

solve2 file = sum . zipWith f operators $ operands
  where
    operators = words . last . lines $ file
    operands =
      map catMaybes . filter (/= [Nothing]) . groupBy (\a b -> isJust a == isJust b) . map readInt . transpose . init . lines $ file

f "*" xs = product xs
f "+" xs = sum xs
f _ _ = undefined
