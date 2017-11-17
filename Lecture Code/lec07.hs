--
-- Tuesday, October 3, 2017
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

{-
   It turns out we need to define rules for submitting homework assignments.
   Otherwise, it is too time consuming to grade ...

   Suggested rules to follow for homework assignments (not finalized)
   *** These rules do naturally not apply to hw03 but will going forwards ***
   
   1) You should run the Hunit tests for each exercise before you submit.
      - if your solution does not run with Hunit test - expect 0 points
      - if your solution to a question does not pass all tests, the points
	    possible for the question will be reduced to match the percentage
		completeness of the solution.
  
   2) For the grading, we need to have your solutions being "in sync" with the
      format of the test files.
      You must submit a zipped file eg: hw04-UIN.zip which contains a solution
	  folder names solutions. In the solutions folder there must be a file for
	  each question for which there is an Hunit test-file
	  
	  As an example, if the Hunit tests had a test file named TestDouble.hs you
	  should submit a solution file in the solutions folder called Double.hs.
	  Each of the solution files (e.g. Double.hs) must have:

            module Main where

      at the top of the module such that Hunit testing works.

      This is the way that you test your solutions yourself.
  
   3) If the code is not
      (1) clean and understandable,
	  (2) with comments explaining the solution,
      (3) nicely formatted
	  you 10% of the total score will be subtracted for each item not fullfilled
  
   4) Before you submit your homework solution, you need to add comments
      that explains how your Haskell function works and why it works. E.g.
      if you write a function fac you write something like:
  
      fac 0 = 1
      fac n = n * (fact (n - 1))
      {-
         This function implements a function that multiplies the numbers
              n*(n-1)* ... *1
         by recursively generating the mult of numbers from n down to 0,
         number by number where 0 terminates the recursion.

         This way the recursive function generates:
              n*(n-1)* ... *1
      -}
	  
   5) If you find your solution in a book, paper, or online, you should add a 
      reference, URL, and/or citation in a comment to the source of your solution
	  AND if you do apply an externally defined solution you should explain how 
	  the solution works! This part is important as it helps document whether you
	  have understood your own solution.
	  
	  Notice that under no circumstances is it permitted to simply copy a complete
	  solution without explanation of where this solution was found and why it is the
	  correct solution.
-}

--
-- You can find my example of how Hunit works here:
--   https://stackoverflow.com/questions/20331209/haskell-unit-testing
--
-- Let's see this in praxis:
--
--    HUnitExample/SafePrelude.hs
--    HUnit_example/TestSafePrelude.hs
--

-- ====================
--
-- Parsing and Grammars
--
-- ====================

import Data.Char
-- import Parsing
import Parsing hiding (expr, term, factor, digit, eval, token, ident, space, identifier)

--
-- Example: parsing tokens
-- 
space :: Parser ()
space = many (sat isSpace) >> 
        return ()

token :: Parser a -> Parser a
token p = space >>
          p >>= \v ->
          space >>
          return v

identifier :: Parser String
identifier = token ident



id_ex00 = parse identifier "abc"
id_ex01 = parse identifier "  abc"
id_ex02 = parse identifier "abc"
id_ex03 = parse identifier "12abc"
id_ex04 = parse identifier "abc12"
id_ex05 = parse identifier "abc is an identifier"

ident :: Parser String
ident = sat isLower >>= \x ->
        many (sat isAlphaNum) >>= \xs ->
        return (x:xs)

id_ex06 = parse ident "  a1234ss  x y"

ident2 :: Parser String
ident2 = token ident

id_ex07 = parse ident2 "  a1234ss  x y"


{- ===================================================== -}
{-
   Want parser for expressions:

   expr   ::= term '+' expr | term
   term   ::= factor '*' term | factor
   factor ::= digit | '(' expr ')'
   digit  ::= '0' | '1' | … | '9' 

-}

expr :: Parser Int -- expr ::= term ('+' expr | epsilon)
expr = do	t <- term 
		do	char '+'
			e <- expr
			return (t + e)
		 +++ return t

term :: Parser Int --- term ::= factor ('*' term | epsilon)
term	=	do	f <- factor 
			do 	char '*'
				t <- term
				return (f * t)
			 +++	return f
	

factor :: Parser Int  --- factor ::= digit | '(' expr ')'
factor  = 	do 	d <- digit
			return (digitToInt d)
		+++
		do	char '('	
			e <- expr
			char ')'
			return e
digit :: Parser Char
digit = sat isDigit

eval   :: String -> Int
eval xs = fst (head (parse expr xs))

exp00 = "3+5*2"
res00 = eval exp00

--
-- We forgot about white space ' '
--
exp01 = "3 + 5 * 2"
res01 = eval exp01

filter_space xs = [ x | x <- xs, x /= ' ' ]

exp02 = filter_space exp01
res02 = eval exp02

-- We will later see how to do this better ...

exp03 = "(3+5)*2"
res03 = eval exp03

-- This shows how you could define an immediate calculater
--
-- But as we learned last time, we could also build a parse tree:
--

-- Define a type to represent the expression 
--   5 + 6 * 8

data Expr 	= Val Int
		| Add Expr Expr
		| Mul Expr Expr
	deriving (Show)

e1 = Add (Val 1) (Mul (Val 2) (Val 3))
e2 = Mul (Val 9) (Add (Val 8) (Val 6))

{-
instance Show Expr where
	show (Val v) = show v
	show (Add s1 s2) = (show s1) ++ "+" ++ (show s2)
	show (Mul s1 s2) = (show s1) ++ "*" ++ (show s2)
-}

--
-- https://wiki.haskell.org/Parsing_expressions_and_statements
--
--    evalExpr01.hs
--