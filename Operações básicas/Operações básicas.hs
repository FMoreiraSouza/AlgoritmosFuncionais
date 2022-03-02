countNeg [] = 0
countNeg a = length [x | x <- a , x < 0]

final n [] = []
final 0 a = [] 
final n a
        |length a >= n = reverse (take n (reverse a))
        |otherwise = []

igual a b c
        |a == b && a == c = 3
        |a == b && a /= c = 2
        |a /= b && b == c = 2
        |a /= b && a == c = 2 
        |otherwise = 0

miolo [] = []
miolo (a:as) 
        |length as == 0 = [a]
        |otherwise = reverse (drop 1 (reverse as))         

gangorra a b c d
        |a * b == c * d = 0
        |a * b < c * d = 1
        |otherwise = -1

min2 a b = min a b

min3 a b c = minimum (a:b:c:[])

soma2 a b =  a + b

fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial(n-1) 

fatorial1 n = product [y | y <- [1..n]]


somaimpares [] = 0
somaimpares a = sum [x | x <- a, mod x 2 /= 0]

max3 a b c = maximum(a:b:c:[])

elemento n [] = 0
elemento n a
        |n > length a || (n < 0 && n * (-1) > length a) = 0
        | n < 0 = (reverse a) !! (n * (-1) - 1)
        |otherwise = a !! (n-1)

divide [] n = ([], [])
divide a n
        |n > length a = ([], [])
        |otherwise = ((take n a), (drop n a))




total [] = 0
total (a:as) = 1 + (total as)

um x = 1

totalvalor [] = 0
totalvalor a = sum (map um a) 

maior [a] = a
maior (a:as)
        |a > maior as = a
        |otherwise = maior as

maiorvalor [a] = a
maiorvalor (a:as) = c
	where c = if a > maior as then a else maior as

corpo [] = []
corpo a = reverse (drop 1 (reverse a)) 

uniao [] [] = []
uniao a [] = a
uniao [] b = b
uniao (a:as) b
        |elem a b = uniao as b
        |otherwise = a : uniao as b

intersec [] [] = []
intersec a [] = []
intersec [] b = []
intersec (a:as) b
        |elem a b =  a:intersec as b
        |otherwise = intersec as b

splitints [] = ([], [])
splitints a = ([y | y <- a, mod y 2 /= 0],[x | x <- a, mod x 2 == 0])


paridade [] = False
paridade a
        |mod (length[x | x <- a, x == True]) 2 == 0 = False
        |otherwise = True


noNeg :: Int -> Int
noNeg n = if n < 0 then n*(-1) else n

getsublist a b (c:cs) p
	| c == b = []
	| c == a = c : getsublist a b cs True
	| p == True = c :getsublist a b cs p
	| otherwise = getsublist a b cs p

sublist :: Int -> Int -> [Int] -> [Int]
sublist a b c = getsublist min' max' c False
        where min' = min x y
              max' = max x y
	      x = noNeg a
	      y = noNeg b


func [] _ _ _ _ = []
func (a:as) x y i l
	| i == x = l !! y : func as x y (i+1) l
	| i == y = l !! x : func as x y (i+1) l
	| otherwise = a : func as x y (i+1) l

swap [] _ _ = []
swap [a] _ _ = [a]
swap a x y = func a x y 0 a

euler1 n = sum[x | x <- [1..n-1], mod x 3 == 0 || mod x 5 == 0]
	
