--
-- Tuesday, September 19, 2017
--

{-
    Important commands:

    :?				Help! Show all commands
    :info
    :load test		      	Open file test.hs or test.lhs
    :reload	              	Reload the previously loaded file
    :main a1 a2   		Invoke main with command line args a1 a2
    :! 	       	       		Execute a shell command
    :edit name			Edit script name
    :edit	    		Edit current script
    :type expr			Show type of expr
    :quit	    		Quit GHCi
-}

import Data.Char

--
-- Possible solutions to HW01 assignment
--

-- 1.
suc n = if (0 < n) then (1 + (suc (n-1))) else 0

increaseTen x = x + 10
increaseTen1 x = 10 + x
-- increaseTen2 x = suc (x + 10)


-- 2.
-- Circle area for circle with radius r is defined as: pi * r^2
--
circleArea r = pi * r^2

-- 3.
-- [a_1,a_2,...,a_m,a_n] ==> [a_2,...,a_n]
--
removeLast []  = []
removeLast [a] = []
removeLast (a:ls) = a:(removeLast ls)

midList ls = if (length ls <= 2) then [] else removeLast (tail ls)

removeLast1 ls = reverse (tail (reverse ls))
midList1 ls = if (length ls <= 2) then [] else removeLast1 (tail ls)

-- 4.
countdownList i j =
   if (j < i) then [] else
      if (j - i) > 0 then (j:(countdownList i (j-1))) else [i]

-- 5.
-- Pythagoras: c^2 = a^2 + b^2
--
isRight a b c =
   if (a > 0) && (b > 0) && (c > 0) then (c^2 == (a^2 + b^2)) else False

-- 6.
-- Complex number multiplication:
--    (a1 + b1i) * (a2 + b2i)  = a1*a2 + (a1*b2)i + (b1*a2)i - b1* b2
--                             = (a1*a2 - b1*b2) + ((a1*b2) + (b1*a2))i
--
multComplex (a1,b1) (a2,b2) = ((a1*a2 - b1*b2), ((a1*b2) + (b1*a2)))

-- 7.
-- Count the number of times character c occurs in string cs
--
countChar c []     = 0
countChar c (a:cs) =
   if (c == a) then (1 + (countChar c cs)) else (countChar c cs)

countChar1 c []     = 0
countChar1 c (a:cs) = n + (countChar1 c cs)
     where n = if (c == a) then 1 else 0
--     where n = if (f c a) then 1 else 0

-- 8.
-- 
getFirsts [] = []
getFirsts ((a,b):ls) = (a:(getFirsts ls))

getFirsts1 []     = []
getFirsts1 (p:ls) = ((fst p):(getFirsts1 ls))

getSeconds1 []     = []
getSeconds1 (p:ls) = ((snd p):(getSeconds1 ls))

t3 = zip "ABCD" (zip [1,2,3,4] "abcd")

-- Flemming to show tuple functions next time:
-- reverseNTuple n nt =
-- isValidTuple nt 

-- 9.
-- Filter out every other element of the list
halfList b []     = []
halfList b (a:ls) =
   if (b == True) then (halfList False ls) else (a:(halfList True ls))

halfListEven ls =  halfList True ls
halfListOdd  ls =  halfList False ls

-- 10.
-- import Data.Char has to be at the top of the file
--
uppercaseList []     = []
uppercaseList (c:cs) =
   (isUpper c, isLower c, isLetter c):(uppercaseList cs)

-- Alternating series using mutual recursion

alternatingSum i []     = 0
alternatingSum i (a:ls) =
   if (even i) then
      (a + (alternatingSum (i+1) ls))
   else
      ((-a) + (alternatingSum (i+1) ls))

altSeries :: Num a => [a] -> a
altSeries ls = alternatingSum 0 ls

-- Closer to mathematical specification, but more expensive
alternatingSum1 i []     = 0
alternatingSum1 i (a:ls) = (((-1)^i)*a) + (alternatingSum1 (i+1) ls)

altSeries1 :: Num a => [a] -> a
altSeries1 ls = alternatingSum1 0 ls
