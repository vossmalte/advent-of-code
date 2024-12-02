import Data.List.Extra

main :: IO ()
main = do
  contents1 <- readFile "6/input-first.txt"
  let ls1 = lines contents1
  let result1 = solution1 ls1
  print result1

  contents <- readFile "6/input.txt"
  let ls = lines contents
  print $ solution1 ls

  -- part 2
  let result2 = discreteDurationWidth $ winningHoldDurationRadius (51926890, 222203111261225)
  print result2

solution1 :: [String] -> Int
solution1 ls = foldl' (*) 1 $ map (discreteDurationWidth . winningHoldDurationRadius) (zipRaces $ map parseLines ls)

parseLines :: String -> [Double]
parseLines = map toDouble . tail . words

zipRaces :: [[b]] -> [(b, b)]
zipRaces [] = []
zipRaces [_] = []
zipRaces (ts : ds : _) = zip ts ds

-- SOLVE WITH MATH
-- distance totalDuration holdDuration = holdDuration * (totalDuration - holdDuration)
-- distanceR totalDuration record holdDuration = holdDuration * (totalDuration - holdDuration) - record
--                       a b      x               = ax - xx - b
--                       quadratic formula solutions:
--                       holdDuration 1/2 = -a/2 +- sqrt(aa/4 - b)
--                       winning holdDuration width = 2 * sqrt(totalDuration*totalDuration/4 - record)
--

winningHoldDurationRadius :: (Double, Double) -> (Double, Double)
winningHoldDurationRadius (totalDuration, record) = (totalDuration / 2, sqrt (totalDuration * totalDuration / 4 - record))

-- 2.5 .. 3.5
discreteDurationWidth :: (Double, Double) -> Int
discreteDurationWidth (middle, width) = ceiling (middle + width) - ceiling (middle - width + 0.0001)

toDouble :: String -> Double
toDouble x = read x :: Double
