--
-- Tuesday, September 12, 2017
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

-- Higher Order Functions

twice f x = f (f x)

map0 f xs = [f x | x <- xs]

-- alternatively

map1 f []     = []
map1 f (x:xs) = f x : map f xs

filter0 p xs = [x | x <- xs, p x]

filter1 p []   = [] 
filter1 p (x:xs)	
   | p x			= x : filter1 p xs	
   | otherwise  		= filter1 p xs

sum0 []     = 0
sum0 (x:xs) = x + sum0 xs

sum1     = foldr (+) 0


product0 []     = 1
product0 (x:xs) = x * product0 xs

product1 = foldr (*) 1

and0 []     = True
and0 (x:xs) = x && (and xs)

and1     = foldr (&&) True

or0 []     = False
or0 (x:xs) = x || (or0 xs)

or1      = foldr (||) False 

foldr0 f v []     = v
foldr0 f v (x:xs) = f x (foldr0 f v xs)

frs1 = foldr0 (+) 0 [1,2,3]
{-
foldr0 (+) 0 [1,2,3]
(+) 1 (foldr0 (+) 0 [2,3])
(+) 1 ((+) 2 (foldr0 (+) 0 [3]))
(+) 1 ((+) 2 ((+) 3 (foldr0 (+) 0 [])))
(+) 1 ((+) 2 ((+) 3 0))
-}

frs2 = foldr0 (-) 0 [1,2,3]
{-
foldr0 (-) 0 [1,2,3]
(-) 1 (foldr0 (-) 0 [2,3])
(-) 1 ((-) 2 (foldr0 (-) 0 [3]))
(-) 1 ((-) 2 ((-) 3 (foldr0 (-) 0 [])))
(-) 1 ((-) 2 ((-) 3 0))
-}

foldl0 f v []     = v
foldl0 f v (x:xs) = foldl0 f (f v x) xs

fls1 = foldl0 (+) 0 [1,2,3]
{-
foldl0 (+) 0 [1,2,3]
foldl0 (+) ((+) 0 1) [2,3]
foldl0 (+) ((+) ((+) 0 1) 2) [3]
foldl0 (+) ((+) ((+) ((+) 0 1) 2) 3) []
           ((+) ((+) ((+) 0 1) 2) 3)

-}

fls2 = foldl0 (-) 0 [1,2,3]
{-
foldl0 (-) 0 [1,2,3]
foldl0 (-) ((-) 0 1) [2,3]
foldl0 (-) ((-) ((-) 0 1) 2) [3]
foldl0 (-) ((-) ((-) ((-) 0 1) 2) 3) []
           ((-) ((-) ((-) 0 1) 2) 3)
-}

{-
   foldr   1 : 2 : 3 : [] => (1 + (2 + (3 + 0)))
   foldl   1 : 2 : 3 : [] => (((0 + 1) + 2) + 3)
-}








-- Exercise 1:

{-
   https://en.wikipedia.org/wiki/Higher-order_function

   In mathematics and computer science, a higher-order function (also
   functional, functional form or functor) is a function that does at
   least one of the following:

      takes one or more functions as arguments (i.e., procedural
      parameters), returns a function as its result.[disputed discuss]

   All other functions are first-order functions. In mathematics
   higher-order functions are also termed operators or
   functionals. The differential operator in calculus is a common
   example, since it maps a function to its derivative, also a
   function.

   In the untyped lambda calculus, all functions are higher-order; in
   a typed lambda calculus, from which most functional programming
   languages are derived, higher-order functions that take one
   function as argument are values with types of the form
   ( τ 1 → τ 2 ) → τ 3

-}

-- Exercise 2:

xs = [0,1,2,3,4,5,6,7,8,9]

f x = x + 1
p x = even x

rs1 = [f x | x <- xs, p x] 

rs2 = map f (filter p xs)



-- Exercise 3:

my_map            :: (a -> b) -> [a] -> [b]
my_map f []       = []
my_map f xs       = foldr (\x xs -> (f x) : xs) [] xs

my_filter             :: (a -> Bool) -> [a] -> [a]
my_filter p []        = []
my_filter p xs        = foldr (\x xs -> if p x then x:xs else xs ) [] xs
