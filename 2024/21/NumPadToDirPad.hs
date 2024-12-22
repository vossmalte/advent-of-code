module NumPadToDirPad where

import Data.List (transpose, sortBy)

acceptButton = 0.5
noneButton = -0.5

up = '^'
down = 'v'
left = '<'
right = '>'

numPad :: [[Float]]
numPad = [[7, 8, 9], [4, 5, 6], [1, 2, 3], [noneButton, 0, acceptButton]]

row n = numPad !! n
col n = map (!! n) numPad

sameRow a b = any (\row -> elem a row && elem b row) numPad
sameCol a b = any (\row -> elem a row && elem b row) (transpose numPad)

-- getNeighbour 'v' a = col !! (head (elemIndices a col) + 1)
--  where
--   col = head (filter (elem a) (transpose numPad))
-- getNeighbour '^' a = col !! (head (elemIndices a col) - 1)
--  where
--   col = head (filter (elem a) (transpose numPad))

getNeighbour :: Char -> Float -> Float
getNeighbour 'v' 1 = error "this is not allowed"
getNeighbour 'v' 2 = 0
getNeighbour 'v' 3 = acceptButton
getNeighbour 'v' a = a - 3
--
getNeighbour '^' (-0.5) = 1
getNeighbour '^' 0 = 2
getNeighbour '^' (0.5) = 3
getNeighbour '^' a = a + 3
--
getNeighbour '>' 0 = acceptButton
getNeighbour '>' (-0.5) = 0
getNeighbour '>' a = a + 1
--
getNeighbour '<' 0 = error "this is not allowed"
getNeighbour '<' (0.5) = 0
getNeighbour '<' a = a - 1

numPadToDirPad :: Float -> Float -> [Char]
numPadToDirPad 0 1 = [up,left]
numPadToDirPad 1 0 = reverse $ numPadToDirPad 0 1
numPadToDirPad 0 4 = [up,up,left]
numPadToDirPad 4 0 = reverse $ numPadToDirPad 0 4
numPadToDirPad 0 7 = [up,up,up,left]
numPadToDirPad 7 0 = reverse $ numPadToDirPad 0 7
numPadToDirPad 0.5 1 = [up,left,left]
numPadToDirPad 1 0.5 = reverse $ numPadToDirPad 0.5 1
numPadToDirPad 0.5 4 = [up,up,left,left]
numPadToDirPad 4 0.5 = reverse $ numPadToDirPad 0.5 4
numPadToDirPad 0.5 7 = [up,up,up,left,left]
numPadToDirPad 7 0.5 = reverse $ numPadToDirPad 0.5 7
numPadToDirPad a b
  | a == b = []
  | sameCol a b = dirCol : numPadToDirPad (getNeighbour dirCol a) b
  | sameRow a b = dirRow : numPadToDirPad (getNeighbour dirRow a) b
  -- if not aligned, just move in col until aligned
  -- | a `elem` row 3 =
  | otherwise = sortBy cmp $ dirCol : numPadToDirPad (getNeighbour dirCol a) b
 where
  dirCol = if a < b then up else down
  dirRow = if a < b then right else left

cmp '<' '<' = EQ
cmp 'v' 'v' = EQ
cmp '<' _ = LT
cmp _ '<'  = GT
cmp 'v' _ = LT
cmp _ 'v'  = GT
cmp _ _ = EQ

controlNumPad' :: [Float] -> [Char]
controlNumPad' [] = []
controlNumPad' [a] = []
controlNumPad' (a : b : xs) = numPadToDirPad a b ++ ['A'] ++ controlNumPad' (b : xs)

controlNumPad :: [Char] -> [Char]
controlNumPad input = controlNumPad' $ (acceptButton :) $ map (\c -> if c == 'A' then acceptButton else read [c] :: Float) input


inverse' currentPos [] = []
inverse' currentPos ('A':rest) = currentPos:inverse' currentPos rest
inverse' currentPos (x:rest) = inverse' (getNeighbour x currentPos) rest

executeNumPad = inverse' acceptButton 
