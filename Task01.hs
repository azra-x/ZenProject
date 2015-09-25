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

foldl'' a m [] = m
foldl'' f a (x:xs) = f (foldl'' f (a) (xs)) x

-- (-) 2 (1,2,3,4)
-- (-) 2 (foldl'' (-) 1 (2,3,4) )
-- (-) 2 ((-) 1 (foldl'' (-) 2 (3,4)))
-- (-)  ((-)1 ((-) 2 ((-) 3 4 )) 2
--pembatas

--foldl1'' a [x] = x
--foldl1'' a (x:xs) = a u (foldl1'' a (m:ml))
--where u = last (x:xs)
--m:ml = tail (reverse xs)

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

scanl'' a u [] = [u]
scanl'' a u (x:xs) = [u] ++ (scanl'' a (a u x) xs)

--pembatas

scanl1' a [] = []
scanl1' a [x] = [x]
scanl1' a (x:xs) = [x] ++ scanl1' a ((a x (head xs)): tail xs)

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

head' (x:xs)
  | x == head xs = takeWhile (==x) (x:xs) ++ dropWhile (==x) (x:xs)

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

words' [] = []
words' (x:xs)
  | x == ' ' = words' xs
  | x == '\n' = words' xs
  | otherwise =

--pembatas

lines' x = x

--pembatas

unlines' [] = []
unlines' (x:xs) = x ++ "\n" ++ (unlines' (xs))

--pembatas

unwords' [] = []
unwords' (x:xs) = x ++ " " ++ (unwords' (xs))

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

inits' [] = [[]]
inits' (x:xs) = (inits' (init (x:xs))) ++ [(x:xs)]

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

--pembatas

intersect' _ [] = []
intersect' [] _ = []
intersect' (x:xs) (y:ys)
  | x == find x (y:ys) = [x] ++ intersect' xs (y:ys)
  | x /= find x (y:ys) = intersect' xs (y:ys)
    where find x [] = 0
          find x (y:ys)
            | x == y = y
            | x /= y = find x (ys)
--pembatas

group' [] = []
group' [x] = [[x]]
group' (x:xs)
  | x == head xs = takeWhile (==x) (x:xs) : group' (dropWhile (==x) (x:xs))
  | otherwise = [x] : group' xs


--how to change list of list into tuple of list

splitAt' a (x:xs) = ((take' a (x:xs)) , (drop' a (x:xs)))

--pembatas

partition' a (x:xs) = ((filter' a (x:xs)) , (unfilter' a (x:xs)))
  where unfilter' _ [] = []
        unfilter' a (x:xs)
          |a x == False = x : unfilter' a xs
          |a x == True = unfilter' a xs

--pembatas

replicate' 0 x = []
replicate' a x = x : replicate' (a-1) x

-- (ceiling, round)

--pembatas
-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
