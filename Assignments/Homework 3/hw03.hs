-- CSCE 314-599: Homework 3
-- Oneal Abdulrahim
-- Due: Thursday, September 28, 2017 @ 11:59p

{- Resources used:
  https://stackoverflow.com
  http://learnyouahaskell.com
  https://haskell.reddit.com
  Lecture Slides
  Graham Hutton, "Programming in Haskell" (2e)
-}

import Data.List
import Data.Char

-- -------------------- Chinese Remainder Theorem (crt) --------------------

{-
The "generateSolutions" (generate brute-force-list) function creates an
infinite list of solutions to the equation a=x%n, where the items in the list
are possible x values.

It takes two integers & the possible upper limit and returns a finite list.
-}
generateSolutions :: Integer -> Integer -> Integer -> [Integer]
generateSolutions a n max = [a, a+n..max]

{-
The "generateSolutions'" (generateSolutions helper) function creates an ordered
list of lists which contains possible solutions to each of the (a, n) tuples to
the equation a=x%n provided as parameters in the form of a list of tuples.

It takes a list of tuples as input and returns a list of lists.
-}
generateSolutions' :: [(Integer, Integer)] -> Integer -> [[Integer]]
generateSolutions' [] _ = []
generateSolutions' (n:ns) max = generateSolutions (fst n) (snd n) max : generateSolutions' ns max

{-
The "maxPossible" function finds the maximum possible number where exactly
one number less than this max satisfies the Chinese Remainder Theorem (crt)
-}
maxPossible :: [(Integer, Integer)] -> Integer
maxPossible [] = 1
maxPossible (n:ns) = (snd n) * maxPossible ns

{-
The "commonIntersection" function finds the common integers across a list of
lists of integers
-}
commonIntersection :: (Foldable t, Eq a) => t [a] -> [a]
commonIntersection ls = foldl1 intersect ls

{-
The Chinese Remainder Theorem "crt" function calculates the unique number x
which satisfies a = x%n.

It takes a list of tuples as inputs and returns a new, single tuple, (a, n)
    a = the smallest # that satisfies the above equation
    n = the product of the n terms
-}
crt :: [(Integer, Integer)] -> (Integer, Integer)
crt [] = (0, 0) -- base case for protection
crt ls = (head $ commonIntersection $ generateSolutions' ls $ maxPossible ls, maxPossible ls)



-- -------------------- k-composite --------------------

{-
The "factors" function generates a list of factors for a given integer, using
"isFactor" helper function.

It takes one integer, and returns a list of ints.
-}
factors :: Integral a => a -> [a]
factors x = [n | n <- [2..x `div` 2], (x `mod` n) == 0] -- skip 1 & self

{-
"kcomposite", given k, produces the ordered (infinite) list of positive
k-composite numbers.

It takes one integer, k, and returns an infinite list of integers
-}
kcomposite ::  Int -> [Int] -- no need to adjust, "factors" already extracts proper factors
kcomposite k = [x | x <- [1..], length (factors x) == k] 
                                                         



-- -------------------- anagram code --------------------
{-
Given plaintext, the "anagramEncode" function implements an encoding procedure, described by:

    1.) If # of letters in message = 2-composite — fine.
    2.) If not, take it up to the next 2-composite length by padding X's
    3.) Arrange the letters in a block.
    4.) The message you send comes from reading down the columns: 
-}

{-
The "padX" function takes a String and pads it with X's until the length is
2-composite.

It takes a String and returns a String.
-}
padX :: [Char] -> [Char]
padX ls | (length $ factors $ length ls) == 2 = map toUpper ls -- normalize
        | otherwise = padX (ls ++ "X")

{-
"Boxes" up a string based on the desired column value
It takes a String and integer (column value) and returns a 2d array of characters
-}
boxString :: [Char] -> Int -> [[Char]]
boxString [] _ = []
boxString ls n = (take n ls) : (boxString (drop n ls) n)

{-
Reads the matrix of characters column-wise.
Takes a 2d array of characters and returns an array of characters (String)
-}
getColumns :: [[Char]] -> [Char]
getColumns ls = head ls ++ getColumns (map (drop 1) ls)

{-
Encrypts messages using algorithm above.
Takes a list of Chars, returns a list of Chars.
-}

anagramEncode :: [Char] -> [Char]
anagramEncode [] = []
anagramEncode ls = getColumns $ boxString (padX ls) $ last $ factors $ length $ padX ls

{-
Decrypts messages using algorithm above.
Takes a list of Chars, returns a list of Chars.

ex: "Tt hroeovrneegr  tithshr aontwo n i ctba ysc tamlnoenn oestyo X bXseX"
                               becomes...
    "There is no castle so strong that it cannot be overthrown by moneyXXX"
-}
anagramDecode:: [Char] -> [Char]
anagramDecode ls = getColumns $ boxString ls $ head $ factors $ length ls
