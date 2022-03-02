import Data.Char
import Data.List.Split

upper [] = []
upper a	= [toUpper x | x <- a ]

isPalind a = a == reverse a

concatmap f [] = []
concatmap f (a:as) = f a ++ concatmap f as

mult (a,b) = a*b

produtoescalar [] [] = 0
produtoescalar a b = sum (map mult (zip a b))


f1 [] c = c
f1 (a:as) c
		|elem a c =  f1 as c 		
		|otherwise = f1 as (c++[a]) 

unique a = f1 a []

tails [] = []
tails u = u : tails (tail u)


indices _ [] = []
indices v a = [y | (x,y) <- filter ((==v).fst) (zip a [0..])]

 
--toUpper' [] = []
--toUpper' (a:as) = (toUpper (a !! 0) : (tail a) ++ " ") : toUpper' as

--toLower' = \x -> map toLower x

--titulo [] = []
--titulo a = map (++) (map toUpper' (splitOn " " (toLower' a)))

select [] _ = []
select _ [] = []
select a (b:bs) = a !! b : select a bs