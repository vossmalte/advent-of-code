module Example where

import Data.Bits
import Prelude hiding (fst)

output = [z00, z01, z02, z03, z04, z05, z06, z07, z08, z09, z10, z11, z12]

f 0 _ = 0
f 1 n = bit n

result :: Int
result = foldr1 (.|.) (zipWith f output [0..])




x00 = 1 :: Int
x01 = 0 :: Int
x02 = 1 :: Int
x03 = 1 :: Int
x04 = 0 :: Int
y00 = 1 :: Int
y01 = 1 :: Int
y02 = 1 :: Int
y03 = 1 :: Int
y04 = 1 :: Int

mjb = xor ntg fgs
tnw = (.|.) y02 x01
z05 = (.|.) kwq kpj
fst = (.|.) x00 x03
z01 = xor tgd rvg
bfw = (.|.) vdt tnw
z10 = (.&.) bfw frj
bqk = (.|.) ffh nrd
djm = (.&.) y00 y03
psh = (.|.) y03 y00
z08 = (.|.) bqk frj
frj = (.|.) tnw fst
z11 = (.&.) gnj tgd
z00 = xor bfw mjb
vdt = (.|.) x03 x00
z02 = (.&.) gnj wpb
kjc = (.&.) x04 y00
qhw = (.|.) djm pbm
hwm = (.&.) nrd vdt
rvg = (.&.) kjc fst
fgs = (.|.) y04 y02
pbm = (.&.) y01 x02
kwq = (.|.) ntg kjc
tgd = xor psh fgs
z09 = xor qhw tgd
kpj = (.|.) pbm djm
ffh = xor x03 y03
ntg = xor x00 y04
z06 = (.|.) bfw bqk
wpb = xor nrd fgs
z04 = xor frj qhw
z07 = (.|.) bqk frj
nrd = (.|.) y03 x01
z03 = (.&.) hwm bqk
z12 = xor tgd rvg
gnj = (.|.) tnw pbm
