-- CSCE 314-599: Homework 1
-- Oneal Abdulrahim
-- Due: Thursday, September 14, 2017 @ 11:59p

{- Resources used:
  https://stackoverflow.com
  http://learnyouahaskell.com
  https://haskell.reddit.com
  Lecture Slides
  Graham Hutton, "Programming in Haskell" (2e)
-}

import Data.Char

{-|
  The 'increaseTen' function adds 10 to a number.
  It takes one argument of type 'Num', and returns a 'Num'
-}
increaseTen :: Num a => a -> a
increaseTen x = x+10 -- add 10 number value

{-|
  The 'circleArea' function computes the area of a circle, given the radius
  It takes one argument of type 'Num', and returns a 'Num'
-}
circleArea :: (Floating a, Ord a) => a -> a
circleArea r | r >=0 = pi*r^2 -- area = pi * radius * radius
             | r < 0 = pi*(abs r)^2 -- account for negative radius (from tests, but r^2 > 0 for all real r)

{-|
  The 'midList' function returns a new list that trims the head and tail of the input list
  It takes one argument of list, and returns a list
-}
midList :: [a] -> [a]
midList [] = [] -- pattern match empty list
midList [n] = [n] -- pattern match one element
midList n = init (tail n) -- remove the first element of the first (length-1) of items

{-|
  The 'countdownList' function gets a list of numbers from the largest to the smallest
  It takes two arguments, both of type 'Num', and returns a list
-}
countdownList :: (Ord a, Enum a) => a -> a -> [a]
countdownList x y | y > x = reverse [x..y] -- get a list in order, then flip it
                  | x >= y = [] -- return empty list if condition isn't met
countdownList' x y = if y > x then reverse [x..y] else [] -- alternative id

{-|
  The 'isRight' function determines if the triangle is a right triangle from its side lengths
  It takes three arguments, all of type 'Num', and returns a 'Bool'
-}
isRight :: (Num a, Ord a) => a -> a -> a -> Bool -- testing for equality, type Ord arises
isRight a b c | a /= 0 && b /= 0 && c/= 0 = c^2 == (a^2 + b^2) -- Pythagorean theorem (parenthesis for clarity)

{-|
  The 'multComplex' function computes the product of two complex numbers a+bi
  It takes two tuple (pair) arguments, and returns a tuple (pair)
-}
multComplex :: (Num a) => (a, a) -> (a, a) -> (a, a)
multComplex (x, y) (u, v) = (x*u-y*v, x*v+y*u) -- formula: (a + bi)*(c + di) = (a*c-b*d)+(a*d+b*c)i

{-|
  The 'countChar' function counts how many times a given character is found in a given string recursively
  It takes two arguments, a 'Char' and a 'String', and returns a 'Num'
-}
countChar :: (Eq a, Num b) => a -> [a] -> b
countChar c s | length s > 0 = if (c == (s !! 0)) then 1 + countChar c (drop 1 s) else countChar c (drop 1 s) -- add 1 if match
              | length s == 0 = 0 -- have to provide a base case. no chars to compare, so add 0
countChar c [] = 0 -- pattern match empty string

{-|
  The 'getFirsts' function recursively finds the first items of a list of tuples
  It takes one argument, a list of two tuples, and returns a list
-}
getFirsts :: [(a, a)] -> [a]
getFirsts x =  map fst x -- map the fst function to every expected tuple in the list x

{-|
  The 'halfList' function recursively removes every even index of a list
  It takes one argument, a list, and returns a list
-}
halfList :: [a] -> [a]
halfList [] = [] -- base case: empty list is even index
halfList [n] = [n] -- base case: single element is first element
halfList (n:ns) = n : halfList (tail ns) -- "prepend" operator separates first element from other elements, tail removes first

{-
  The 'upperCaseList' function takes a string and determines whether it has an upper case, lower case letter, or digit
  It takes one argument, a 'String', and returns a list of Boolean tuples (a tuple for every char)
-}

-- need a helper function to make a decision for each char, using built-in functions on chars
decideChar :: Char -> (Bool, Bool, Bool)
decideChar c | isUpper c = (True, False, False)
             | isLower c = (False, True, False)
             | isDigit c = (False, False, True)
             | otherwise = (False, False, False)

-- method described first above :)
upperCaseList :: String -> [(Bool, Bool, Bool)]
upperCaseList [] = [] -- pattern match empty case
upperCaseList n = map decideChar n -- map & perform decision making helper function on all elements

{-
  The 'altSeries' function computes an alternating series of an input list
  It takes one argument, a list, and returns a 'Num'
-}
altSeries :: Num a => [a] -> a
altSeries [] = 0 -- base case: no items left to add
altSeries (n:ns) = n + (-1) * altSeries (ns) -- add head, then -1*tail. recursion inverts the correct terms (-1)*(-1)