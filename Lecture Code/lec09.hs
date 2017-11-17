--
-- Tuesday, October 10, 2017
--
-- module Main where

import Data.Char
import Prelude hiding (sum)
import Data.List (foldl')

--
-- Midterm Grading deadline Oct 16 ... what will count for you is:
--    Homework assignments 3, done - grading uploaded by end of the week
--    Midterm 1, October 12 - grading uploaded no later than Oct 16.
--

--
-- Question: What will be the curriculum for TAMU midterm test?
-- Answer:   Expect selected parts of what we learned until now ...
--

--
-- Today: Lazy evaluation and Modules ...
--

--
-- Modules, Abstract Data Trees (ATD), Recursive Data Types
--

-- ListStackADT.hs
-- StackADT.hs
-- stackUser.hs
--
-- Tree.hs
-- treeUser.hs
--

--
-- Examples of Converting Imperative (C, Java, ...) programs to Haskell
--
{-

   Fibonacci numbers: are a series of numbers in which each number is
   the sum of the two preceding numbers.

   The simplest is the series 1, 1, 2, 3, 5, 8, etc.

   A single Fibonacci number can be calculated in C like this:

   int fib (int n)
   {
     int a = 0,
         b = 1,
         i, temp;

     for (i = 0; i < n; i++)
     {
    	temp = a + b;
	a = b;
	b = temp;
     }
     return a;
   }
-}

--
-- In Haskell this could be:
--
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-
   And it gets more complex if we want to generate a list of Fibonazzi numbers

   ... in C:

   int * fibList(int n)
   {
     int * fibs;
     fibs = (int *)malloc((sizeof int) * n);
     for (i = 0; i < n; i++)
     {
    	fibs[i] = a;
	temp = a + b;
        a = b;
        b = temp;
     }
     return fibs;
   }

-}

--
--    In Haskell this can now be defined very elegantly:
--

fibs n = map fib [0..n]

fib20 = fibs 20              -- Notice: really 21 numbers!
len20 = length fib20

fibInfinity = map fib [0..]

fib20' = take 20 fibInfinity -- actual 20 numbers
len20' = length fib20'


{-
   Another example calculating NORMS where

     norm(x) = sqrt(sum(mapSq(x)))

   In C we have:

   void mapSq(double *x, double *y, int n)
   {
     int i;
     for(i=0; i<n; i++)
     {
       y[i] = x[i] * x[i];
     }
   }

   double sum(double *x, int n)
   {
     double result = 0;
     int i;
     for(i=0; i<n; i++)
     {
       result += x[i];
     }
     return result;
   }

   double norm1(double *x, int n)
   {
     double *y = malloc(n * sizeof(double));
     mapSq(x, y, n);
     theSum = sum(y, n);
     free(y);
     return sqrt(theSum);
   }
-}

--
-- In Haskell we can simple write this as:
--

mapSq x = map sq x
  where
  sq xi = xi * xi

sum1 x = foldl' (+) 0.0 x
norm x = (sqrt . sum1 . mapSq) x

norm01 = print (norm [1,2,3,4,5])
norm02 = (norm [1,2,3,4,5])
norm03 = norm (take 10 [1..])
