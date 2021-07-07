primos :: Int -> Bool
primos 2 = True
primos 3 = True
primos n = length [x | x <- [2..div n 2], mod n x == 0] == 0

gerar :: Int -> [Int]
gerar n = [x | x <- [2..n], primos x]

maxpot :: Int -> Int -> Int
maxpot 0 _ = 0
maxpot n x
  | mod n x == 0 = 1 + maxpot (div n x) x
  | otherwise = 0

ax :: Int -> [(Int,Int)] -> Int -> [(Int,Int)]
ax _ [] s = []
ax n (a:as) s
  | t == n = (x,y):[]
  | x == 0 || y == 0 = ax n as s
  | otherwise = (x,y):ax n as t
  where t = s * (x ^ y)
        x = fst a
        y = snd a

factors :: Int -> [(Int,Int)]
factors n = ax n a 0
  where a = [(x, maxpot n x) | x <- gerar n]


import Data.Char

alphabet = ['a'..'z']

index' _ [] = -1
index' a (b:bs) = if toLower a == toLower b then 0 else 1 + index' a bs

calc a b = mod (index' a alphabet + index' b alphabet) 26

get_letter x = if x >= 0 && x < 26 then alphabet !! x else ' '

vigenere [] _ = []
vigenere a [] = []
vigenere (a:as) (b:bs)
 | isUpper a = toUpper (get_letter (calc a b)) : vigenere as bs
 | otherwise = get_letter (calc a b) : vigenere as bs

main = print (vigenere "ATACARBASESUL" "LIMAOLIMAOLIM")