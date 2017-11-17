{-
    Course: CSCE 314 Section 599, midterm exam 1
    Date:   October 12, 2017
    Author: Flemming Andersen
 
    Description:
	    One possible solution to the midterm questions
-}

import System.IO
import Data.List

-- 1)

tripler f x = f( f ( f x ))
-- tripler :: (t -> t) -> t -> t

-- 2)

pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack [x]    = [[x]]
pack (x:xs) =
   if x `elem` (head (pack xs))
     then (x:(head (pack xs))):(tail (pack xs))
     else [x]:(pack xs)

pk01 = pack "hhheeeelloowwwoorrrrlllddd"
-- ["hhh","eeee","ll","oo","www","oo","rrrr","lll","ddd"]

encode :: Eq a => [a] -> [(Int, a)]
encode xs =
  (enc . pack) xs
         where enc = foldr (\x acc -> (length x, head x) : acc) []

enc' xss = foldr (\x acc -> (length x, head x) : acc) [] xss

en01 = enc' ["hhh","eeee"]
-- [(3,'h'),(4,'e')]

ans02 = encode "hhheeeelloowwwoorrrrlllddd"
-- [(3,'h'),(4,'e'),(2,'l'),(2,'o'),(3,'w'),(2,'o'),(4,'r'),(3,'l'),(3,'d')]

-- 3)

a_3 = map (* 5) [1..5] 
-- [5,10,15,20,25]

b_3 = take 5 [x * x - 1 | x <- [1..]] 
-- [0,3,8,15,24]
--
-- [0,3,8,15,24,..]

b_3_1 = take 5 [x * x | x <- [1..]] 

b_3_2 = map (\x -> x - 1) (take 5 [x * x | x <- [1..]])

-- 3 * 4 + 5 * 6 - 17
-- (3 * 4) + (5 * 6) - 17

c_3   = filter (`elem` "aeiou") "facetious"
c_3'  = filter (\e -> e `elem` "aeiou") "facetious"
c_3'' = filter (\e -> elem e "aeiou") "facetious"
-- "aeiou"

is_elem01 e = e `elem` "aeiou"

-- not_is_elem e = e `elem` "aeiou"

c_3''' = filter is_elem01 "facetious"
-- c_3'''' = filter not_is_elem "facetious"

is_elem02 e = elem e "aeiou"

a_in_string = is_elem01 'a'

d_3 = (\x -> [y | y <- [1..x], (mod x y) == 0]) 24
-- [1,2,3,4,6,8,12,24]

-- :type [[1, 2, 3], [4, 5, 6]]
-- [[1, 2, 3], [4, 5, 6]] :: Num t => [[t]]

-- :type [['a'], ['b','c']]
-- [['a'], ['b','c']] :: [[Char]]

-- 4)

f4 x = (4 * x + 3)/(5 - 2 * x)
g4 y = (5 * y - 3)/(4 + 2 * y)

ans04 = f4 $ g4 $ f4 $ g4 3.0
-- 3.0

-- 4)

-- :type zip'
-- zip' :: [t] -> [t1] -> [(t, t1)]
--
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- :type repeat'
-- repeat' :: a -> [a]
--
repeat' x = x : repeat' x

rep x = x : rep x

-- a. zip' :: [Int] -> [Int] -> [Int] WRONG
-- b. zip' :: [Int] -> [Int] -> [(Int,Int)] CORRECT
-- c. zip' :: [a] -> [b] -> [(a,b)] CORRECT AND MOST GENERAL
-- d. zip' :: [a] -> [b] -> [(b,a)] WRONG
-- e. zip' :: [a] -> [a] -> [(a,a)] CORRECT
-- f. zip' :: (Eq a) => [a] -> [a] -> [(a,a)] CORRECT

-- Which of the above types, if any, is the most general type for zip'?
-- c

-- a. repeat' :: [Int] -> [Int] WRONG
-- b. repeat' :: (Eq a) => a -> [a] CORRECT
-- c. repeat' :: [a] -> [[a]] CORRECT
-- d. repeat' :: a -> a WRONG
-- x. repeat' :: a -> [a] CORRECT AND MOST GENERAL

-- Which of the above types, if any, is the most general type for repeat'?
--   None of them!
-- The most general type is:
--   repeat' :: a -> [a]

--
-- This is actually a less general type because it is resetricted to
-- types a that satisfies (Eq a)
--
repeat'' :: (Eq a) => a -> [a]
repeat'' x = x : repeat'' x

--
-- The is also not the most general type as it requires that the argument is
-- of type list:
--
repeat''' :: [a] -> [[a]]
repeat''' x = x : repeat''' x

-- 5)

a_5 = zip [1,2,3] [10,11]
-- [(1,10),(2,11)]
-- Notice that zip works fine here,
-- it just zips up to the lenght of the shortest list
--

b_5 = zip "squid" "clams"
-- [('s','c'),('q','l'),('u','a'),('i','m'),('d','s')]

c_5 = zip [1..] [10..]
-- take 5 c_6
-- [(1,10),(2,11),(3,12),(4,13),(5,14), ..]

d_5 = zip (repeat True) (repeat "squid")
-- take 3 d_6
-- [(True,"squid"),(True,"squid"),(True,"squid"), ..]

-- e_5 = zip (1,2,3) (10,11,12)
-- type error!

-- 6)

curry' :: ((a, b) -> c) -> a -> b -> c
-- curry' f = \x y -> f (x, y)
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
-- uncurry' f = \(x,y) -> f x y
uncurry' f (x,y) = f x y

uncurry''  f x = f (fst x) (snd x)
uncurry''' f x = (fst x) `f` (snd x)

-- 7)

echo' =
   do
      j <- readLn
      k <- readLn
      putStrLn ("the sum is " ++ show (j+k))

-- 
-- echo''' j k = putStrLn ("the sum is " ++ show (j+k))
--
-- * This is not correct, it does not get j k from readLn:

--
-- Remember >>= that we introduced in lecture 6 (lec06a.hs)?
--
-- :type (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
--
-- :type readLn
-- readLn :: Read a => IO a
--
-- :type putStrLn
-- putStrLn :: String -> IO ()

--
-- So this is correct:
--
echo'' =
   readLn >>= \j -> readLn >>=
   \k -> putStrLn ("the sum is " ++ show (j+k))

-- 8)

-- :type map
-- map :: (a -> b) -> [a] -> [b]
--

map2  f x y = \ms -> map ( \(x,y) -> f x y) ms
map2' f x y = \ms -> map ( \(x,y) -> f (x, y)) ms

map2''  f n (x:xs) = (f n x) : (map2'' f n xs)
map2''' f   (x:xs) = (f (fst x) (snd x)) : (map2''' f xs)

-- not right: map2IV f g xs    = map g ( map f xs )
--            map2V  f (x:xs) (y:ys) = (f x y) . map2IV xs ys

-- 9)

data Color = RGB Int Int Int
                 deriving (Show, Eq)

isRBColor :: Color -> Bool
isRBColor a | a == RGB 148 0 211 = True
            | a == RGB 75  0 130 = True
            | a == RGB 0   0 255 = True
            | a == RGB 0 255   0 = True
            | a == RGB 255 255 0 = True
            | a == RGB 255 127 0 = True
            | a == RGB 255   0 0 = True
            | otherwise = False

t9 = isRBColor (RGB 148   0 211) &&
     isRBColor (RGB  75   0 130) &&
     isRBColor (RGB   0   0 255) &&
     isRBColor (RGB   0 255   0) &&
     isRBColor (RGB 255 255   0) &&
     isRBColor (RGB 255 127   0) &&
     isRBColor (RGB 255   0   0)


-- 10)

--
-- Smarter then my original solution
--
comLen s | (reverse s) == s = ((length s) `div` 2)
         | otherwise        = -1

--
-- Remember from HW02:
--
isPalindrome [ ] = True
isPalindrome [a] = True
isPalindrome  al =
  ((head al) == (last al)) && (isPalindrome (init (tail al)))

--
-- Same principle used:
--
commonLength [ ] = 0
commonLength [a] = 0
commonLength  al =
   if ((head al) == (last al)) then
    (1 + (commonLength (init (tail al))))
   else -1

p01 = commonLength "racecar"
-- 3
p02 = commonLength "redder"
-- 3
p03 = commonLength "motor"
-- -1

-- 11)

an01 = 153
as01 = [1, 5, 3]

an02 = 1634
as02 = [1, 6, 3, 4]

digits 0     = []
digits x     = digits (x `div` 10) ++ [x `mod` 10]

armDigits ds = map (\x -> x^(length ds)) ds

isFixNum n = (n == (sum (armDigits (digits n))))

nArms n = [ a | a <- [1..(10^n)], isFixNum a, ((length (digits a)) == n) ]

ex0 = \n -> map (^n) [1..10]

nArms' n = [ x | x <- [1..(1000)], (x == sum (map (^n) [ y | y <- [1..(1000)], z <- [1..(1000)], (y == (x `mod` (10^z))) ] ))]

-- ---------

getDigits :: Int -> [Int]
getDigits 0 = []
getDigits x = (x `mod` 10) : getDigits (x `div` 10)

--
-- Check:
--
armGD04 = map getDigits [1634, 8208, 9474] 
-- [[4,3,6,1],[8,0,2,8],[4,7,4,9]]

armD04  = map digits [1634, 8208, 9474]
-- [[1,6,3,4],[8,2,0,8],[9,4,7,4]]

-- aNum :: Int -> Int -> Int
-- aNum x n = (map (+) ((getDigits x)^n))

-- aNum x n = ((map (+) (getDigits x)^n) == x)

-- nArms' n = [ x | x <- [10^(n-2)+ 1..10^(n-1)], (map (+) (getDigits x)^n == x) ]


-- 12)

data Tree a = Leaf a | Branch2 (Tree a) (Tree a) | BranchN [Tree a]
   deriving (Show, Eq, Read, Ord)

leafBranch :: Tree a -> [a]
leafBranch (Leaf l) = [l]
leafBranch (Branch2 left right) = leafBranch left ++ leafBranch right
leafBranch (BranchN ls)         = leafBranchL ls

leafBranchL     [] = [] 
leafBranchL (l:ls) = (leafBranch l) ++ (leafBranchL ls)

tb1 = Leaf 0
tb2 = Branch2 (Leaf 1) (Leaf 2)
tb3 = BranchN [tb1, tb2]
tb4 = Branch2 tb3 (Leaf 3)

isLeaf    (Leaf a)    = True
isLeaf          _     = False

isBranch2 (Branch2 a b) = True
isBranch2            _  = False

isBranchN (BranchN a) = True
isBranchN          _  = False

leafVal (Leaf a) = a

leafBranch2 t = if (isBranch2 t) then (leafBranch t) else []
leafBranchN t = if (isBranchN t) then (leafBranch t) else []

