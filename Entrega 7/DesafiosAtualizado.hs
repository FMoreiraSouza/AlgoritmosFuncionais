import Data.Char
import Data.List

--Auxiliares
pares c 
    |mod c 2 == 0 = -c
    |otherwise = c

expoente n = 2^n

--Geradores

gerador1 = 0:[g | g <- [1 ..], g <- [g, -g]] 

gerador2 = [pares g | x <- [1 ..]]

gerador3 = (1:[  expoente g |  g <- [1 ..]])

gerador4 n = takeWhile ( > 0) (iterate (\g -> div g 2) n)

gerador5 n = unfoldr (\e -> if e == 0 then Nothing else Just (e, div e 2)) n

--Dígitos

digitos n = unfoldr (\(a, b) -> if (div a 10 , div b 10) == (0,0) then Nothing else Just (b, (b ,div a 10))) (mod n 10, div n 10)

--Fatores

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

fatores :: Int -> [(Int,Int)]
fatores n = ax n a 0
  where a = [(x, maxpot n x) | x <- gerar n]

--Vigenere

alfabeto = ['A'..'Z']

f :: Maybe Int -> Int
f x =
 case x of
  Just x -> x
  Nothing -> -1

calc letra1 letra2 = mod (indexL1 + indexL2) 26
 where
   indexL1 = f (elemIndex letra1 alfabeto)
   indexL2 = f (elemIndex letra2 alfabeto)

get_letter x = if x >= 0 && x < 26 then alfabeto !! x else ' '

toUpper' a = map toUpper a

vigenere [] _ = []
vigenere a [] = []
vigenere a b = map (\x -> (get_letter (calc (fst x) (snd x)))) (zip (toUpper' a) (toUpper' b))

 
