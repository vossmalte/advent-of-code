module Main where

import DirPadToDirPad -- (controlDirPad, inverse)
import NumPadToDirPad -- (controlNumPad)

input =
  [ "964A"
  , "140A"
  , "413A"
  , "670A"
  , "593A"
  ]

produceHumanInput = controlDirPad . controlDirPad . controlNumPad

getNumericPart = (\x -> read x :: Int) . init

hash instruction code = length instruction * getNumericPart code

solve input = sum $ zipWith hash (map produceHumanInput input) input

main :: IO ()
main = do
  let x = map (execute . produceHumanInput) input
  putStrLn $ "Part 1: " ++ show (solve input)

-- testing

inputTest =
  [ "029A"
  , "980A"
  , "179A"
  , "456A"
  , "379A"
  ]

expectedTest =
  [ "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"
  , "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A"
  , "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
  , "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A"
  , "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
  ]

exe1 = executeDirPad
exe2 = executeDirPad . executeDirPad
execute = executeNumPad . executeDirPad . executeDirPad

-- the first one goes from 2 to 9 in a weird way
checkNumPad = zipWith (==) (map exe2 expectedTest) (map controlNumPad inputTest)

-- here everything is good
checkDirPad1 = zipWith (==) (map executeDirPad expectedTest) (map (controlDirPad . exe2) expectedTest)

checkDirPad2 = zipWith (==) expectedTest (map (controlDirPad . controlDirPad . exe2) expectedTest)

checkHashes = zipWith (==) (zipWith hash expectedTest inputTest) (zipWith hash (map produceHumanInput inputTest) inputTest)

checkLength = zipWith (==) (map length expectedTest) (map (length . produceHumanInput) inputTest)

r = "v<<A>>^AAAvA^A<vA<AA>>^AvAA<^A>Av<<A>A>^AAAvA<^A>A<vA>^A<A>A"
e = "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A"
