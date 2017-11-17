-- CSCE 314-599: Homework 7
-- Oneal Abdulrahim
-- Due: Thursday, November 2, 2017 @ 11:59p

{- Resources used:
  https://stackoverflow.com
  http://learnyouahaskell.com
  https://haskell.reddit.com
  Lecture Slides
  Graham Hutton, "Programming in Haskell" (2e)
-}

-- D:\OneDrive\Documents\TAMU\"Computer Science"\"CSCE 314"\Assignments\"Homework 7"
-- Please see TestSuite.hs for a list of tests and executable module Main
{-
How to run tests:

>>:l hw07.hs
>>:a TestSuite.hs
>>:main

-} 

module HW7 where
import Prelude hiding (lookup)

-- AST definition for W
------ Data Values ------
data WValue = VInt Int 
            | VBool Bool 
              deriving (Eq, Show)
 
------ Language Expressions ------
data WExp = Val WValue                   -- single value
 
          | Var String                   -- variable values
 
          | Plus WExp WExp               -- adding
          | Minus WExp WExp              -- subtracting
          | Multiplies WExp WExp         -- multiplying
          | Divides WExp WExp            -- dividing
 
          | Equals WExp WExp             -- ==
          | NotEqual WExp WExp           -- /=
          | Less WExp WExp               -- < (strictly)
          | Greater WExp WExp            -- > (strictly)
          | LessOrEqual WExp WExp        -- <=
          | GreaterOrEqual WExp WExp     -- >=
 
          | And WExp WExp                -- &&
          | Or WExp WExp                 -- ||
          | Not WExp                     -- ~ (inverter)

------ Statements ------
data WStmt = Empty                       -- null
           | VarDecl String WExp         -- variable declaration using expression
           | Assign String WExp          -- variable initialization
           | If WExp WStmt WStmt         -- if-block (only if)
           | While WExp WStmt            -- while-block
           | Block [WStmt]               -- a segment of code (one line)

------ Interpreter ------
type Memory = [(String, WValue)]
marker = ("|", undefined)
isMarker (x, _) = x == "|"

-- helper functions for use with evaluator below
{-
Set of integer operations which can be performed by using W expressions.
This following includes helper functions for + (_plus), - (_minus),
* (_multiply), / (_divide)
@param  x    VInt operands
        y
@return VInt The result of the operation
-}
_plus :: WValue -> WValue -> WValue
_plus (VInt x) (VInt y) = VInt $ x + y
_plus _ _ = error "!!! Error parsing plus operation\n\n"

_minus :: WValue -> WValue -> WValue
_minus (VInt x) (VInt y) = VInt $ x - y
_minus _ _ = error "!!! Error parsing minus operation\n\n"

_multiply :: WValue -> WValue -> WValue
_multiply (VInt x) (VInt y) = VInt $ x * y
_multiply _ _ = error "!!! Error parsing multiply operation\n\n"

_divide :: WValue -> WValue -> WValue
_divide (VInt x) (VInt y) = VInt $ x `div` y
_divide _ _ = error "!!! Error parsing divide operation\n\n"

{-
Set of logical operations which can be performed by using W expressions.
This following includes helper functions for == (_equals), /= (_notEqual),
< (_less), > (_greater), <= (_lessOrEqual), >= (_greaterOrEqual)
@param  x     VInt operands
        y
@return VBool The logical result of the boolean operation
-}
_equals :: WValue -> WValue -> WValue
_equals (VInt x) (VInt y) = VBool $ x == y
_equals _ _ = error "!!! Error parsing equality operation\n\n"

_notEqual :: WValue -> WValue -> WValue
_notEqual (VInt x) (VInt y) = VBool $ x /= y
_notEqual _ _ = error "!!! Error parsing inequality operation\n\n"

_less :: WValue -> WValue -> WValue
_less (VInt x) (VInt y) = VBool $ x < y -- x `_less` y (order matters)
_less _ _ = error "!!! Error parsing less than operation\n\n"

_greater :: WValue -> WValue -> WValue
_greater (VInt x) (VInt y) = VBool $ x > y -- x `_greater` y (order matters)
_greater _ _ = error "!!! Error parsing less than operation\n\n"

_lessOrEqual :: WValue -> WValue -> WValue
_lessOrEqual (VInt x) (VInt y) = VBool $ x <= y -- order matters
_lessOrEqual _ _ = error "!!! Error parsing less than/equal operation\n\n"

_greaterOrEqual :: WValue -> WValue -> WValue
_greaterOrEqual (VInt x) (VInt y) = VBool $ x >= y -- order matters
_greaterOrEqual _ _ = error "!!! Error parsing less than/equal operation\n\n"

{-
Set of boolean operations which can be performed by using W expressions.
This following includes helper functions for && (_and), || (_or), not (_not)
@param  x     VBool operands
        y
@return VBool The logical result of the boolean operation
-}
_and :: WValue -> WValue -> WValue
_and (VBool x) (VBool y) = VBool $ x && y
_and _ _ = error "!!! Error parsing AND operation\n\n"

_or :: WValue -> WValue -> WValue
_or (VBool x) (VBool y) = VBool $ x || y
_or _ _ = error "!!! Error parsing OR operation\n\n"

_not :: WValue -> WValue
_not (VBool x) = VBool $ not x
_not _ = error "!!! Error parsing NOT operation\n\n"

{-
Evaluator function, properly passes arguments to helper functions used above.
Pattern matching is somewhat exhaustive. We make assumptions about correct input syntax.
@param   WExp   The operation and input arguments
         Memory The current memory items on "stack"
@return         Evaluated value after memory is recursively evaluated OR
                Error if statement fails
-}
eval :: WExp -> Memory -> WValue
eval (Val x) z = x
eval (Var str) z = case (lookup str z) of -- Using Maybe as a Monad
                 Just y -> y          -- https://wiki.haskell.org/Maybe
                 Nothing -> error ("!!! Could not find " ++ str ++ "!\n\n")
eval (Plus x y) z = _plus (eval x z) (eval y z)
eval (Minus x y) z = _minus (eval x z) (eval y z)
eval (Multiplies x y) z = _multiply (eval x z) (eval y z)
eval (Divides x y) z = _divide (eval x z) (eval y z)
eval (Equals x y) z = _equals (eval x z) (eval y z)
eval (NotEqual x y) z = _notEqual (eval x z) (eval y z)
eval (Less x y) z = _less (eval x z) (eval y z)
eval (Greater x y) z = _greater (eval x z) (eval y z)
eval (LessOrEqual x y) z = _lessOrEqual (eval x z) (eval y z)
eval (GreaterOrEqual x y) z = _greaterOrEqual (eval x z) (eval y z)
eval (And x y) z = _and (eval x z) (eval y z)
eval (Or x y) z = _or (eval x z) (eval y z)
eval (Not x) z = _not $ eval x z

{-
Execution function, executes memory items such as declarations, assigments, blocks.

@param   WStmt   A statement keyword (see data type above)
         Memory  The current memory items on "stack"
@return          Evaluated expression added to memory stack OR
                 Error if statement fails
-}
exec :: WStmt -> Memory -> Memory
exec Empty x = x -- if nothing to execute, pass memory
exec (VarDecl str we) z | (lookup str z) == Nothing = (str, (eval we z)) : z -- add declaration after evaluating, only if new
                        | otherwise = error ("!!! Found instance of declaration for " ++ str ++ "\n\n")
exec (Assign str we) z | (lookup str z) == Nothing = error ("!!! No instance of declaration for " ++ str ++ "\n\n")
                       | otherwise = (str, (eval we z)) : z -- add assignment after evaluating, only if declared
exec (Block []) z = z -- empty block, we are finished. just pass memory
exec (Block (x:xs)) z = exec (Block xs) $ exec x z
exec (If we1 str1 str2) z | (eval we1 z) == VBool True = exec str1 z -- if first condition is true
                          | (eval we1 z) == VBool False = exec str2 z -- else
exec (While we str) z | (eval we z) == VBool True = exec (While we str) $ exec str z -- keep passing loop lines
                      | otherwise               = z -- once loop condition is not met, exit & return current memory stack
 
-- some useful helper functions (given)
lookup s [] = Nothing
lookup s ((k,v):xs) | s == k = Just v
                    | otherwise = lookup s xs

asInt (VInt v) = v
asInt x = error $ "Expected a number, got " ++ show x

asBool (VBool v) = v
asBool x = error $ "Expected a boolean, got " ++ show x

fromJust (Just v) = v
fromJust Nothing = error "Expected a value in Maybe, but got Nothing"
