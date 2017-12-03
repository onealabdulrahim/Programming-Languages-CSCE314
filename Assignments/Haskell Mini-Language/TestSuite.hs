-- CSCE 314-599: Homework 7 Test Suite
-- Oneal Abdulrahim
-- Due: Thursday, November 2, 2017 @ 11:59p

{-
            ---- Some Unit Tests ----
-}

module Main where
import HW7
import Prelude hiding (lookup)
import Test.HUnit
import System.Exit

-- factorial (given in specs)
factorial = 
  Block
  [
    VarDecl "acc" (Val (VInt 1)),
    While (Greater (Var "x") (Val (VInt 1)))
    (
      Block
      [
        Assign "acc" (Multiplies (Var "acc") (Var "x")),
        Assign "x" (Minus (Var "x") (Val (VInt 1)))         
      ]
    )
  ]

-- p1 (given in specss)
p1 = Block
     [
       VarDecl "x" (Val (VInt 0)),
       VarDecl "b" (Greater (Var "x") (Val (VInt 0))),
       If (Or (Var "b") (Not (GreaterOrEqual (Var "x") (Val (VInt 0)))))
         ( Block [ Assign "x" (Val (VInt 1)) ] )
         ( Block [ Assign "x" (Val (VInt 2)) ] )
     ]

-- testing general logic and syntax of W
test1 = Plus (Val (VInt 10)) (Val (VInt 10))              -- 10 + 10
test2 = Minus (Val (VInt 10)) (Val (VInt 10))             -- 10 - 10
test3 = Multiplies (Val (VInt 10)) (Val (VInt 10))        -- 10 * 10
test4 = Divides (Val (VInt 100)) (Val (VInt 10))          -- 100 `div` 10
test5 = Equals (Val (VInt 10)) (Val (VInt 100))           -- 10 == 100 ?
test6 = NotEqual (Val (VInt 10)) (Val (VInt 10))          -- 10 ~= 10 ?
test7 = Greater (Val (VInt 100)) (Val (VInt 10))          -- 100 > 10 ?
test8 = GreaterOrEqual (Val (VInt 10)) (Val (VInt 10))    -- 10 >= 10 ?
test9 = Less (Val (VInt 10)) (Val (VInt 100))             -- 10 < 100 ?
test10 = LessOrEqual (Val (VInt 100)) (Val (VInt 10))     -- 100 <= 10 ?
test11 = And (Val (VBool True)) (Val (VBool False))       -- True && False ?
test12 = Or (Val (VBool False)) (Val (VBool False))       -- False || False ?
test13 = Not $ Val $ VBool False                          -- ~False

{-
Calculates the n-th fibonacci number in the sequence.
@param  n       n-th digit (as VInt)
@return result  n-th digit in the sequence (as VInt)
-}
fibonacci = Block
      [
        VarDecl "result" (Val (VInt 1)),
        VarDecl "fst" (Val (VInt 0)),
        VarDecl "snd" (Val (VInt 1)),
        VarDecl "i" (Val (VInt 1)),
        While ( Less (Var "i") (Var "n"))
        (
          Block [
                  Assign "result" (Plus (Var "fst") (Var "snd")),
                  Assign "fst" (Var "snd"),
                  Assign "snd" (Var "result"),
                  Assign "i" (Plus (Var "i") (Val (VInt 1)))
                ]
        )
      ]

-- intentional failures
fail1 = Plus (Val (VBool True)) (Val (VInt 10))           -- illegal: Bool + Int
fail2 = Assign "x" $ Val $ VInt 3
fail3 = Block
      [
        VarDecl "x" (Val (VInt 1)),
        VarDecl "x" (Val (VInt 2))
      ]

-- unit tests
myTestSuite =
    TestList [
      test $ assertEqual "Testing... p1" 2 (asInt (fromJust (lookup "x" (exec p1 [])))),
      let res = lookup "acc" (exec factorial [("x", VInt 10)])
      in test $ assertBool "factorial of 10" (3628800 == asInt (fromJust res)),
  
      " Testing... Addition" ~: asInt (eval test1 []) ~=? 20,
      " Testing... Subtraction" ~: asInt (eval test2 []) ~=? 0,
      " Testing... Multiplication" ~: asInt (eval test3 []) ~=? 100,
      " Testing... Division" ~: asInt (eval test4 []) ~=? 10,
      " Testing... Equal" ~: asBool (eval test5 []) ~=? False,
      " Testing... Not Equal" ~: asBool (eval test6 [])~=? False,
      " Testing... Greater Than" ~: asBool (eval test7 []) ~=? True,
      " Testing... Greater Than or Equal To" ~: asBool (eval test8 [])~=? True,
      " Testing... Less Than" ~: asBool (eval test9 []) ~=? True,
      " Testing... Less Than or Equal To" ~: asBool (eval test10 []) ~=? False,
      " Testing... And" ~: asBool (eval test11 []) ~=? False,
      " Testing... Or" ~: asBool (eval test12 []) ~=? False,
      " Testing... Not" ~: asBool (eval test13 []) ~=? True,
      " Testing... Fibonacci" ~: asInt (fromJust (lookup "result" (exec fibonacci [("n", VInt 5)]))) ~=? 5,
      " Intentional FAIL 1! THIS FAILURE IS INTENTIONAL" ~: asBool (eval fail1 []) ~=? False,
      " Intentional FAIL 2! THIS FAILURE IS INTENTIONAL" ~: asBool (fromJust (lookup "x" (exec fail2 []))) ~=? True,
      " Intentional FAIL 3! THIS FAILURE IS INTENTIONAL" ~: asBool (fromJust (lookup "x" (exec fail3 []))) ~=? True
      ]

-- main: run the unit tests  
main = do c <- runTestTT myTestSuite
          putStrLn $ show c
          let errs = errors c
              fails = failures c
          putStrLn "\n\n\nTHERE ARE A TOTAL OF 19 CASES, 16 PASS, 3 INTENTIONAL FAIL\n\n\n"
          if (errs + fails /= 0) then exitFailure else return ()