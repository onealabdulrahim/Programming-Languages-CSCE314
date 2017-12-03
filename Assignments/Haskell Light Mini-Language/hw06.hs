-- CSCE 314-599: Homework 6
-- Oneal Abdulrahim
-- Due: Thursday, October 26, 2017 @ 11:59p

{- Resources used:
  https://stackoverflow.com
  http://learnyouahaskell.com
  https://haskell.reddit.com
  Lecture Slides
  Graham Hutton, "Programming in Haskell" (2e)
-}

-- D:\OneDrive\Documents\TAMU\"Computer Science"\"CSCE 314"\Assignments\"Homework 6"

{-
Abstract data type E, wherein literals, operators, equality instances
are used to represent expressions for use in abstract syntax trees (AST).
-}
data E = IntLit Int
       | BoolLit Bool
       | Plus E E
       | Minus E E
       | Multiplies E E
       | Exponentiate E E
       | Equals E E
         deriving (Eq, Show)

{-
Set of helper functions for use in evaluator below
-}
_Plus (IntLit x) (IntLit y) = IntLit $ x+y           -- add
_Plus (BoolLit x) (BoolLit y) = BoolLit $ x||y       -- x `or` y
_Minus (IntLit x) (IntLit y) = IntLit $ x-y          -- fst minus snd
_Multiplies (IntLit x) (IntLit y) = IntLit $ x*y     -- multiply
_Multiplies (BoolLit x) (BoolLit y) = BoolLit $ x&&y -- x `and` y
_Exponentiate (IntLit x) (IntLit y) = IntLit $ x^y   -- fst raised to snd
_Equals (IntLit x) (IntLit y) = BoolLit $ x==y       -- check for equality
_Equals (BoolLit x) (BoolLit y) = BoolLit $ x==y     -- check for equality

{-
Evaluator for the mini-language E, as defined above.
-}
eval :: E -> E
eval (IntLit i)         = IntLit i
eval (BoolLit b)        = BoolLit b
eval (Plus x y)         = _Plus x y
eval (Minus x y)        = _Minus x y
eval (Multiplies x y)   = _Multiplies x y
eval (Exponentiate x y) = _Exponentiate x y
eval (Equals x y)       = _Equals x y

-- regular log function
_log :: Int -> Int
_log x | x == 1 = 0
       | x == 2 = 1
       | x `mod` 2 == 0 && x > 0 = 1 + _log(x `div` 2)

log2Sim :: E -> E
log2Sim (IntLit x) = IntLit (_log x)
log2Sim (Equals x y) = Equals (log2Sim x) (log2Sim y)
log2Sim (Exponentiate x y) = Multiplies y $ log2Sim x
log2Sim (Multiplies x y) = Plus (log2Sim x) (log2Sim y)
log2Sim (Minus x y) = log2Sim (eval (Minus x y))
log2Sim (Plus x y) = log2Sim (eval (Plus x y))