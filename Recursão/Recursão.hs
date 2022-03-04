fib 0 = 0 
fib 1 = 1 
fib n = (fib (n-1)) + (fib (n-2))

freq _[] = 0
freq n (a:as) = if n == a then 1 + freq n as else freq n as

unico c n
	|length (filter(==c) n) == 1 = True
	|otherwise = False


f1 _ [] = []
f1 n (a:as) = if n == a then a : f1 n as else f1 n as

unicorecursivo n [] = False
unicorecursivo n a = if length (f1 n a) == 1 then True else False

maioresQue _ [] = []
maioresQue n (a:as) = if a > n then a : maioresQue n as else maioresQue n as


concatena [] [] = []
concatena [] b = b
concatena a [] = a
concatena (a:as) b = a:(concatena as b)

menores a (b:bs)
	|b < a = b:(menores a bs)
	|otherwise = (menores a bs)

alter 0 = []
alter n = alter (n-1) ++ [n,(-n)]

reverso [] = []
reverso (a:as) = (reverso as) ++ [a]

intercalar [] [] = []
intercalar [] b = b
intercalar a [] = a
intercalar (a:as) (b:bs) = a:b: intercalar as bs


func i n s = if i < n then s : func (i+1) n (s+1)  else []
sequencia x y = func 0 x y
		
moveresq 0 a = a
moveresq n a = moveresq (n-1) (tail a ++ [head a])

moverdir 0 a = a
moverdir n a = moverdir (n-1) ([last a] ++ init a)


f2 i n 
		|i*i == n = True 
		|i*i > n = False
		|otherwise = f2(i+1) n

quad n = f2 1 n

dlt n [] = []
dlt b (a:as) = 
	if b == a
	then as 
	else a:dlt b as   

func1 i [] = []
func1 i a
 | i <= length a = sum (take i a) : func1 (i+1) a 
 | otherwise = []
 
acm [] = []
acm a = func1 1 a

func2 i n s = if i < n then s : func2 (i+1) n (s+1)  else []

line n = func 0 n (sum [1..n-1] + 1)

triangle 0 = []
triangle n = triangle (n-1) ++ [line n]
