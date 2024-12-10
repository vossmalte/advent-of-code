module Solve2 where

import Data.List
import Input2

decode :: Int -> [Int] -> [(Int, Int)]
decode _ [] = []
decode id [x] = [(id, x)]
-- free = -1
decode id (x : y : xs) = (id, x) : (-1, y) : decode (id + 1) xs

isFile = ((-1) /=) . fst

compress :: [(Int, Int)] -> [(Int, Int)]
compress [] = []
-- compress disk = foldl' (flip insertFile) disk $ files disk
compress disk = foldr insertFile disk $ filter isFile disk

solve2 = sum . zipWith (*) [0 ..] . flatten . compress . decode 0

files disk = reverse $ filter isFile disk

removeFile :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
removeFile file [] = []
removeFile file@(i, _) ((i', s') : restDisk)
  | i == i' = (-1, s') : restDisk
  | otherwise = (i', s') : removeFile file restDisk

insertFile :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
-- no disk = done
insertFile file [] = []
-- next free space has size 0 = delete it
insertFile file ((-1, 0) : restDisk) = insertFile file restDisk
-- next free space
insertFile (id, fileSize) ((-1, spaceSize) : restDisk)
  -- it fits: put it there and the remaining space and the rest of the disk without the file
  | fileSize == spaceSize = (id, fileSize) : removeFile (id, fileSize) restDisk
  | fileSize < spaceSize = (id, fileSize) : (-1, spaceSize - fileSize) : removeFile (id, fileSize) restDisk
  | otherwise = (-1, spaceSize) : insertFile (id, fileSize) restDisk
insertFile (id1, s1) ((id2, s2) : restDisk)
  -- next block is the file itself = abort
  | id1 == id2 = (id2, s2) : restDisk
  -- next block is some file = leave it
  | otherwise = (id2, s2) : insertFile (id1, s1) restDisk

-- https://stackoverflow.com/questions/20568276/implement-insert-in-haskell-with-foldr
para :: (a -> [a] -> r -> r) -> r -> [a] -> r
para c n (x : xs) = c x xs (para c n xs)

insertFileP :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertFileP file@(id, size) disk = takeWhile isFile disk ++ para inst [] (dropWhile isFile disk)
 where
  inst (i, s) restDisk acc
    | i == id = file : restDisk
    | i == -1 && s == size = (id, size) : removeFile file restDisk
    | i == -1 && s > size = (id, size) : (-1, s - size) : removeFile file restDisk
    | otherwise = (i, s) : acc

insertFile' :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertFile' file [] = []
insertFile' file@(id, size) disk = foldr insertF [] $ init $ tails disk
 where
  insertF ((i, s) : xs) acc
    | i == id = file : xs
    | i == -1 && s == size = (id, size) : removeFile file xs
    | i == -1 && s > size = (id, size) : (-1, s - size) : removeFile file xs
    | otherwise = (i, s) : acc

flatten = concatMap expand
 where
  expand (i, s)
    | i == -1 = replicate s 0
    | otherwise = replicate s i

-- debug stuff

steps disk = disk : zipWith insertFile' (files disk) (steps disk)

prettify = concat . concatMap str
 where
  str (i, s)
    | i == -1 = replicate s "."
    | otherwise = replicate s (show i)

debug = mapM_ (putStrLn . prettify) . steps . decode 0
