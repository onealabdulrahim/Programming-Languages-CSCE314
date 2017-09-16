--
-- Tuesday, September 05, 2017
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

a = 15 ; -- semicolons are not needed, but ok
b = 3 

foo x = 50 * x

foo' x y = sqrt(x^2 + y^2) 	-- this raises type mismatch errors if we do
				--	foo' a b


cube :: Integer -> Integer
cube x = x^3

{-
 - cube 2 works just fine
 - cube -2 freaks out, solution cube (-2)
 -}

x1 :: Integer
x1 = 7
x2 :: Int
x2 = 7

cube2 :: Int -> Int
cube2 x = x^3

pow9 x = cube (cube (x))

r1 :: Double
r1 = 3.0


r2 :: Float
r2 = 3.0
{-

-}
-- compare = (r1 == r2) -- Haskell is strict about comparing double and float too.

sillySgn :: Integer -> Integer
sillySgn x = if x==0 then 0 else
		if x < 0 then -1 else 1

-- can write: if boolean then "string" else "another string"
-- b = -5
-- testing: if b > 0 then [5,6,7] else []

sillySgn' :: Integer -> Integer
sillySgn' x = if x==0 then 0 else
		if x==1 || x == (-1) then x else
			if x < 0 then sillySgn' (x+1) else sillySgn' (x-1)

f 0 = "hello" -- Reverse order generates a warning. A helpful one at that.
f x = f (x-1)


{-
s x 	| x == 0   = "blah"
	| otherwise = "other value"
-}

myPairEx = (4, "hello") -- pair, 2-tuple
myTripEx = (True, 5, 6.7) -- triple, 3-tuple
myTripEx2 = (5, 6.7, "foo") -- triple, 3-tuple

-- tuples vs. lists
-- pairs: fst -> first, snd -> second

g pr = (fst pr) + 20 -- can take a pair as input
makeTrip n = (n, n*2, n*3)

adder x y = x+y

adder2(x,y) = x+y

incr = adder 1 -- adder is more useful


omit x = 0
-- keep_going 50 = "done"
keep_going x = keep_going (x+1)

{-
  keep_going 12
  keep_going (12+1)
  keep_going 13
  keep_going (13+1)
	...

  keep_going 12
  keep_going (12+1)
  keep_going ((12+1)+1)
  keep_going (((12+1)+1)+1)
	...

-}

keep_going2 x = if (x <= 0) then [0] else (x:(keep_going2 (x-1)))

-- Laziness is a virtue:
infl = [1..]
liml x = (take x infl)

g_revl n = if (n <= 1) then [1] else (n:(g_revl (n-1)))
genl n = reverse (g_revl n)

genl2 n = if (n <= 1) then [1] else ((genl2 (n-1))++[n])

genl3 n | n == 0 = [0]
genl3 n | n >  0 = ((genl3 (n-1)) ++ [n])

la = genl  4
lb = genl2 4

bab n = ((genl  n) == (genl2 n))

lam = \x -> x + 1

lam1 x = x + 1

lamEq = (lam 4) == ((\x -> x + 1) 4)

l = ["abc", "defg", "ijklm"]

h x y = x + 2*y

--  map (h 100) [3..10]



