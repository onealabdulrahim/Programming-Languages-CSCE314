--
-- Tuesday, October 5, 2017
--

module Main where

import Data.Char

--
-- WhyTestEnvHasToBeCorrect.hs
--

--
-- Midterm Grading deadline Oct 16 ... what will count for you is:
--    Homework assignments 3, done - grading uploaded by end of the week
--    Midterm 1, October 12 - grading uploaded no later than Oct 16.
--

--
-- What will be the curriculum for TAMU midterm grading:
--   All we have learned so far ... which is:
--
-- Some of you asked whether we would rehurse the material visited so far ...
--
-- A walk through lecture code ...
--
-- If you have specific questions for the midterm next week - sent me questions
-- NO LATER than Sunday noon.
-- 

{- ================================================================== -}
{-           Some solutions to homework 3                             -}
{- ================================================================== -}

{- *** Chinese Reminder Theorem *** -}
--
-- Apply brute force to find a common x 
--
-- a0 = x mod n0
-- ...
-- ai = x mod ni
--

--
-- Returns list of modulo values up to bound
--
returnMod :: Integer -> Integer -> Integer -> [Integer]
returnMod a c bound = [ b | b <- [1..bound], (a-b) `mod` c == 0]

c  = 7 * 5 * 3 -- 105
x1 = returnMod 2 7 c
x2 = returnMod 1 5 c
x3 = returnMod 0 3 c

x1_eq = (x1 == [2,9,16,23,30,37,44,51,58,65,72,79,86,93,100])
x2_eq = (x2 == [1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,81,86,91,96,101])
x3_eq = (x3 == [3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,
                66,69,72,75,78,81,84,87,90,93,96,99,102,105])

--
-- Check if element is in list
--
isElem :: Eq a => a -> [a] -> Bool
isElem a []     = False
isElem a (x:xs) = if a == x then True else isElem a xs

--
-- This function returns two smallest common from two lists
--
common :: [Integer] -> [Integer] -> Integer
common [] _       | True             = (-1)
common _ []       | True             = (-1)
common (x:xs) ys  | isElem x ys      = x
                  | not(isElem x ys) = common xs ys

--
-- Combine two mods
--
combineMods :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
combineMods xs ys | xs == ys = xs
combineMods (a,b) (c,d) = (k, z)
                         where
                            z=b*d
                            k = common (returnMod a b z) (returnMod c d z)
--
-- Using our functions:
--
crt :: [(Integer, Integer)] -> (Integer, Integer)
crt (x:xs)   | length xs /= 0 =  crt ((combineMods x (head xs)): (tail xs))
             | otherwise      =  combineMods (0,1) x


{-
   Check from assignment:
-}
ex31 = crt [(2, 7), (0, 3), (1, 5)]
