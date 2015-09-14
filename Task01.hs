-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
module Template where

import Data.List

-- 1.a

null' a
  | a == [] = True
  | a /= [] = False

--pembatas

take' 0 x = []
take' x [] = []
take' u (x:xs) = x : take' (u-1) (xs)

--pembatas

drop' 0 x = x
drop' x [] = []
drop' u (x:xs) = drop (u-1) (xs)

--pembatas

fst' (a,b) = a

--pembatas

snd' (a,b) = b

--pembatas

map' u [] = []
map' u (x:xs) = u x : map' u xs

--pembatas

filter' _ [] = []
filter' a (x:xs)
  | a x == True = x : filter' a xs
  | a x == False = filter' a xs

--pembatas

delete' u [] = []
delete' u (x:xs)
  | u == x = xs
  | otherwise = x : delete' u xs

--pembatas

deleteAll' u [] = []
deleteAll' u (x:xs)
  | u == x = deleteAll' u xs
  | u /= x = x : deleteAll' u xs


--pembatas

foldel a f [] = f
foldel a f (x:xs) = a f (foldel a x (xs))

--pembatas

foldel1 a [x] = x
foldel1 a (x:xs) = a x (foldel1 a xs)

--pembatas

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' (xs) (ys)

--pembatas

zipWith' a [] [] = []
zipWith' a (x:xs) [] = []
zipWith' a [] (x:xs) = []
zipWith' a (x:xs) (y:ys) = a x y : zipWith' a (xs) (ys)

--in progress

nth' (x:xs) a
  | a == 0 = x
  | a >= 0 = nth' (xs) (a-1)

--pembatas

--scanl' a u [] = [u]
--scanl' a u (x:xs) = a u (scanl' a u xs)

--pembatas

scanl1' x = x

--pembatas

elem' u [] = False
elem' u (x:xs)
  | u == x = True
  | u /= x = elem' u xs
  | otherwise = False

--pembatas

notElem' u [] = True
notElem' u (x:xs)
  | u == x = False
  | u /= x = notElem' u xs
  | otherwise = True

--pembatas

head' (x:xs) = x

--pembatas

length' [] = 0
length' (x:xs) = 1 + (length' xs)

--pembatas

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ (x:[])

--pembatas

last' [x] = x
last' (x:xs) = last' xs
--pembatas

tail' (x:xs) = xs

--pembatas

init' [x] = []
init' (x:xs) = x : init' xs

--pembatas

max' x y
  | x > y = x
  | x < y = y
  | x == y = x

--pembatas

min' x y
  | x > y = y
  | x < y = x
  | x == y = x
--pembatas

concat' [] = []
concat' [[]] = []
concat' (x:xs) = x ++ concat' xs

--pembatas

intersperse' a [] = []
intersperse' a [x] = [x]
intersperse' a (x:xs) = x : a : intersperse' a (xs)

--pembatas

intercalate' (x:xs) (y:ys) = (y:ys)

--pembatas

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' xs

--pembatas

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = or' xs

--pembatas

zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' (xs) (ys) (zs)

--pembatas

sum' [] = 0
sum' (x:xs) = x + (sum' xs)

--pembatas

product' [] = 1
product' (x:xs) = x * (product' xs)

--pembatas

words' x = x

--pembatas

lines' x = x

--pembatas

unlines' x = x

--pembatas

unwords' x = x

--pembatas

takeWhile' _ [] = []
takeWhile' a (x:xs)
  | a x == False = []
  | a x == True = x : takeWhile' a xs
  | otherwise = takeWhile' a xs

--pembatas

dropWhile' _ [] = []
dropWhile' a (x:xs)
  | a x == False = (x:xs)
  | a x == True = dropWhile' a (xs)
  | otherwise = dropWhile' a (xs)

--pembatas

concatMap' x = x

--pembatas

all' _ [] = True
all' a (x:xs)
  | a x == False = False
  | otherwise = all' a (xs)

--pembatas

any' _ [] = False
any' a (x:xs)
  | a x == True = True
  | otherwise = any' a (xs)

--pembatas

insert' _ [] = []
insert' u (x:xs)
  | u <= x = u:x:xs
  | otherwise = x : insert' u xs

--pembatas

zipWith3' a [] [] [] = []
zipWith3' a (x:xs) (z:zs) [] = []
zipWith3' a [] (x:xs) (z:zs) = []
zipWith3' a (x:xs) [] (z:zs) = []
zipWith3' a (x:xs) (y:ys) (z:zs) = a x y z : zipWith3 a (xs) (ys) (zs)

--pembatas

-- 1.b

nub' [] = []
nub' (x:xs) = x : nub' (deleteAll' x (x:xs))

--pembatas

sort' [x] = [x]
sort' [] = []
sort' (x:xs) = minimum' (x:xs) : sort' xs

--pembatas

minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

--pembatas

maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

--pembatas

inits' x = x

--pembatas

tails' [] = [[]]
tails' (x:xs) = [x:xs] ++ tails' xs

--pembatas

union' [] [] = []
union' [x] [] =  [x]
union' [] [x] = [x]
union' [x] [y] = [x,y]
union' (x:xs) (y:ys)
  | x == y = x : union' (xs) (ys)

--baris terakhir = kenapa tdk bisa kebalikan?

intersect' [] [] = []
intersect' [x] [] = []
intersect' [] [x] = []
intersect' (x:xs) (y:ys)
  | x == y = x : intersect' (x:xs) (ys)

--pembatas

group' [] = []
group' (x:xs) = [x] : group' (xs)

--how to change list of list into tuple of list

splitAt' a (x:xs) = ((take' a (x:xs)) , (drop' a (x:xs)))

--pembatas

partition' _ [] = [] ++ [[]]

--pembatas

replicate' 0 x = []
replicate' a x = x : replicate' (a-1) x

--pembatas
-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
