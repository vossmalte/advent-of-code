module CmpTest where
cmp 47 53 = LT
cmp 53 47 = GT
cmp 97 13 = LT
cmp 13 97 = GT
cmp 97 61 = LT
cmp 61 97 = GT
cmp 97 47 = LT
cmp 47 97 = GT
cmp 75 29 = LT
cmp 29 75 = GT
cmp 61 13 = LT
cmp 13 61 = GT
cmp 75 53 = LT
cmp 53 75 = GT
cmp 29 13 = LT
cmp 13 29 = GT
cmp 97 29 = LT
cmp 29 97 = GT
cmp 53 29 = LT
cmp 29 53 = GT
cmp 61 53 = LT
cmp 53 61 = GT
cmp 97 53 = LT
cmp 53 97 = GT
cmp 61 29 = LT
cmp 29 61 = GT
cmp 47 13 = LT
cmp 13 47 = GT
cmp 75 47 = LT
cmp 47 75 = GT
cmp 97 75 = LT
cmp 75 97 = GT
cmp 47 61 = LT
cmp 61 47 = GT
cmp 75 61 = LT
cmp 61 75 = GT
cmp 47 29 = LT
cmp 29 47 = GT
cmp 75 13 = LT
cmp 13 75 = GT
cmp 53 13 = LT
cmp 13 53 = GT
cmp _ _ = EQ
