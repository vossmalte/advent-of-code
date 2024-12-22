module DirPadToDirPad where

import Data.List (transpose)

up = '^'
down = 'v'
left = '<'
right = '>'

getNeighbour 'v' '^' = 'v'
getNeighbour 'v' 'A' = '>'

dirPadToDirPad :: Char -> Char -> [Char]
--
dirPadToDirPad '<' '<' = ""
dirPadToDirPad '<' 'v' = ">"
dirPadToDirPad '<' x = '>' : dirPadToDirPad 'v' x
dirPadToDirPad '>' '>' = ""
dirPadToDirPad '>' 'A' = "^"
dirPadToDirPad '>' 'v' = "<"
dirPadToDirPad '>' x = '<' : dirPadToDirPad 'v' x
dirPadToDirPad 'A' '>' = "v"
dirPadToDirPad 'A' 'A' = ""
dirPadToDirPad 'A' '^' = "<"
dirPadToDirPad 'A' 'v' = "<v"
dirPadToDirPad 'A' '<' = "v<<"
dirPadToDirPad '^' 'A' = ">"
dirPadToDirPad '^' '^' = ""
dirPadToDirPad '^' 'v' = "v"
dirPadToDirPad '^' x = 'v' : dirPadToDirPad 'v' x
dirPadToDirPad 'v' '<' = "<"
dirPadToDirPad 'v' '>' = ">"
dirPadToDirPad 'v' 'A' = ">^"
dirPadToDirPad 'v' '^' = "^"
dirPadToDirPad 'v' 'v' = ""

inverseDir2Dir :: Char -> [Char] -> Char
inverseDir2Dir '<' "" = '<'
inverseDir2Dir '<' ">" = 'v'
inverseDir2Dir '>' "" = '>'
inverseDir2Dir '>' "<" = 'v'
inverseDir2Dir '>' "^" = 'A'
inverseDir2Dir 'A' "" = 'A'
inverseDir2Dir 'A' "<" = '^'
inverseDir2Dir 'A' "v" = '>'
inverseDir2Dir '^' "" = '^'
inverseDir2Dir '^' ">" = 'A'
inverseDir2Dir '^' "v" = 'v'
inverseDir2Dir 'v' "" = 'v'
inverseDir2Dir 'v' "<" = '<'
inverseDir2Dir 'v' ">" = '>'
inverseDir2Dir 'v' ">^" = 'A'
inverseDir2Dir 'v' "^" = '^'
inverseDir2Dir x [] = x
inverseDir2Dir x [ys] = error $ show "no match for " ++ [x] ++ " " ++ show ys
inverseDir2Dir x (y : ys) = inverseDir2Dir (inverseDir2Dir x [y]) ys

--

inverse' currentPos [] = []
inverse' currentPos input = nextPos : inverse' nextPos (tail end)
 where
  nextPos = inverseDir2Dir currentPos start
  (start, end) = span (/= 'A') input

executeDirPad = inverse' 'A'

controlDirPad' :: [Char] -> [Char]
controlDirPad' [] = []
controlDirPad' [a] = []
controlDirPad' (a : b : xs) = dirPadToDirPad a b ++ "A" ++ controlDirPad' (b : xs)

controlDirPad = controlDirPad' . ('A' :)


-- breaking
x = inverseDir2Dir '<' ">>^"
