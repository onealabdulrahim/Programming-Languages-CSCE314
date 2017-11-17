--
-- Tuesday, September 21, 2017
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

--
-- An example og how to implement a Java "for" iteration statement in Haskell
-- 

--
--   Java for loop example:
--   ======================
--
{-
	sumSoFar = 0;
	for (int i = 0; i < lst.size(); i++)
	{
		sumSoFar += lst[i];
		output[i] = sumSoFar;
	}
-}

--
-- Example in Haskell:
--

l5  = [1..5]
l10 = [1..10]
ls  = "abcde"

cSum :: [Integer] -> [Integer]
cSum lst = cSumHelper 0 0 lst

cSumHelper sumSoFar i lst =
   if i < (length lst) then
      sumSoFar':(cSumHelper sumSoFar' (i+1) lst)
   else
      []
   where
      sumSoFar' = sumSoFar + (lst!!i)

--
-- Using function parameters to compare length
-- Notice that ls is here an implicit parameter
--
sameLength :: [a] -> [b] -> Bool
sameLength ls ms = length ls == length ms

--
-- You can also define your own operators:
--
(~#~) :: [a] -> [b] -> Bool
(~#~) ls ms = length ls == length ms

--
-- sameLength ls l5
-- (~#~) ls l5
-- you can also check:
-- sameLength ls l5 == (~#~) ls l5
--

-- We have seen that:
--    String == [Char]
--
-- Similarly Haskell allow us to define new types in terms of old types:
--    type <NewType> = <OldType>

--
-- If we want to represent polynomials like:
--    a*(x^n) + b*(x^m) + ... 
-- we may encode this as a list of values f = [a,b,...] where
--    n = (length f), a = f!!0 and m = (length f - 1), b = f!!1, ...
-- Hence, we can define a type Polynomial like this:
--
type Polynomial = [Double]

xs = [2,0,7,16]

f :: Polynomial
f = [2,0,7,16]

p :: Polynomial
p = [18,0,17,-6]
-- p = 18*x^3 + ... (-6)*(x^0)

q :: Polynomial
q = [1,3]

--
-- Now to evaluate the value of the Polynomial we can write:
--
evalPoly :: Polynomial -> Double -> Double
evalPoly [] x = 0.0
evalPoly p x = (head p)*x^e + evalPoly (tail p) x
			where e = (length p)-1

--
-- Now we can specialize our functions to represent p, q, ... polynomials:
--
polyP x = evalPoly p x
polyQ x = evalPoly q x

--
-- Your exercise:
--    Try to write the above without the ^ 
--

--
-- If we want to add two polynomials p1, p2 up to the length if the shortest
--
addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly p1 p2 = addPolyHelper (padToLen p1 l) (padToLen p2 l) 
				where l = max (length p1) (length p2)

addPolyHelper p1 p2 = map (\x -> (fst x + snd x))(zip p1 p2)

padToLen :: Polynomial -> Int -> Polynomial
padToLen p len 	| (length p) == len  	=	p
		| otherwise		= 	padToLen (0.0:p) len

--
-- Your exercise:
--   Try to write the above where you all to the min length of p1, p2
--

{-
   We just looked at how we can define type synonyms using the keyword
   "type".
   However, we can also introduce new types using the keyword "data".

   The Haskell keywords "type" and "data" are different, though:
       "data" allows you to introduce a new algebraic data type, while
       "type" just makes a type synonym
-}

{-
   data <TypeName> = A|...
-}

{-
   Bool is an example of a predefined data type:

   :info Bool
   data Bool = False | True 	-- Defined in `GHC.Types'
   instance Flippable Bool -- Defined at lec04b.hs:418:10
   instance Bounded Bool -- Defined in `GHC.Enum'
   instance Enum Bool -- Defined in `GHC.Enum'
   instance Eq Bool -- Defined in `GHC.Classes'
   instance Ord Bool -- Defined in `GHC.Classes'
   instance Read Bool -- Defined in `GHC.Read'
   instance Show Bool -- Defined in `GHC.Show'
-}

--
-- Our own data type example:
--
data RainbowColors = RB_Red   | RB_Orange | RB_Yellow | 
                     RB_Green | RB_Blue   | RB_Indigo | RB_Violet
     deriving (Show)

--
-- Notice without Show we cannot print the values ...
--
rb_color = RB_Green

-- Another example:
--
-- Kleene's "3-valued" logic
--
data Tern = F | U | T

tNot :: Tern -> Tern
tNot F = T
tNot T = F
tNot U = U

tAnd :: Tern -> Tern -> Tern
tAnd F _ = F
tAnd _ F = F
tAnd T T = T
tAnd _ _ = U

tOr :: Tern -> Tern -> Tern
tOr F F = F
tOr T _ = T
tOr _ T = T
tOr _ _ = U

--
-- Notice that we cannot directly "print" values without adding Show to Tern
--
tVal = U

--
-- but, we can still "print" by using this method: 
--
s :: Tern -> String
s F = "False"
s U = "Unknown"
s T = "True"

-- 
--
-- Let us revisit foldr and foldl that we looked at in lecture 3
--
-- foldr can be understood as:
--
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

--
-- foldl can be understood as:
--
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

-- Alternatively you can understand it this way:
{-
   Right recursion:
   ----------------
      foldr   1 : 2 : 3 : [] => (1 + (2 + (3 + 0)))

   Left recursion:
   ----------------   
   foldl   1 : 2 : 3 : [] => (((0 + 1) + 2) + 3)
-}

-- So, how can we e.g. program <length of a list> via foldr
-- foldrLen ls = ...
--
l1 = [5,8,2,19]

-- First attempt:
op x y = x + y
foldrLen ls = foldr op 0 ls
-- fails as it gives the sum not the length
-- [5,8,2,19] -> 5:8:2:19:[]
--            -> 5 `op` (8 `op`  (2 `op` (19 `op` 0)))
fat1 = foldrLen [5,8,2,19]

-- second attempt - works:
-- Notice that the first argument is disregarded ...
op2 _ y = 1 + y
foldrLen2 ls = foldr op2 0 ls
-- [5,8,2,19] -> 5:8:2:19:[]
--            -> 5 `op2` (8 `op2`  (2 `op2` (19 `op2` 0)))
fat2 = foldrLen2 [5,8,2,19]

-- Third attempt:
op3 x y = 1 + x
foldrLen3 ls = foldr op3 0 ls
-- [5,8,2,19] -> 5:8:2:19:[]
--            -> 5 `op3` (8 `op3`  (2 `op3` (19 `op3` 0)))
fat3 = foldrLen3 [5,8,2,19]

-- Fourth attempt using foldl - works:
op4 x y = 1 + x
foldlLen4 ls = foldl op4 0 ls
-- [5,8,2,19] -> 5:8:2:19:[]
--            -> ((((0 `op4` 5) `op4` 8) `op4` 2) `op4` 19)
fat4 = foldlLen4 [5,8,2,19]

-- Fifth attempt using foldl - fails:
op5 x y = 1 + y
foldlLen5 ls = foldl op5 0 ls
-- [5,8,2,19] -> 5:8:2:19:[]
--            -> ((((0 `op5` 5) `op5` 8) `op5` 2) `op5` 19)
fat5 = foldlLen5 [5,8,2,19]

--
-- Example of avoiding laziness
--
lenA [] = 0
lenA (_:xs) = 1 + lenA xs
{- 
 - 	lenA [1,9,6,4] (want evaluation for each recursion step)
 -	  = 1 + lenA [9,6,4]
 -	  = 1 + 1 + lenA [6,4]
 -	  = 1 + 1 + 1 + lenA [4]
 -	  = 1 + 1 + 1 + 1 + lenA []
 -	  = 1 + 1 + 1 + 1 + 0
 -	  = 4
 -}

lenB ls = lenHelper 0 ls
		where 	lenHelper c [] = c
			lenHelper c (_:xs) = lenHelper (c+1) xs	
{-
 - 	lenB [1,9,6,4] (want evaluation for each recursion step)
 - 		= lenHelper 0 [1,9,6,4]
 - 		= lenHelper 1 [9,6,4]
 - 		= lenHelper 2 [6,4]
 - 		= lenHelper 3 [4]
 - 		= lenHelper 4 []
 - 		= 4
 -          ^
 - --- This | isn't quite right.
 -
  - 	lenB [1,9,6,4] (want evaluation for each recursion step)
 - 		= lenHelper 0 [1,9,6,4]
 - 		= lenHelper (0+1) [9,6,4]
 - 		= lenHelper ((0+1)+1) [6,4]
 - 		= lenHelper (((0+1)+1)+1) [4]
 - 		= lenHelper ((((0+1)+1)+1)+1) []
 - 		= ((((0+1)+1)+1)+1)
 - 		= 4
 -
-}

h a b = b
-- seq a b = b (but also, get me the value of a)

--
-- Using a `seq` b enforces evaluation of expr a before expr b
--
lenC ls = lenHelper 0 ls
		where 	lenHelper c [] = c
			lenHelper c (_:xs) = let s = (c+1) 
						in s `seq` (lenHelper s xs)

{-
 - 	lenC [1,9,6,4] (want evaluation for each recursion step)
 - 		= lenHelper 0 [1,9,6,4]
 - 		= lenHelper 1 [9,6,4]
 - 		= lenHelper 2 [6,4]
 - 		= lenHelper 3 [4]
 - 		= lenHelper 4 []
 - 		= 4
 -
-}

--
-- Defining your own types and classes - deriving Show, Read, Eq, Ord, ...
--

data Gate = Ajar | Shut
--	deriving (Show)
	deriving (Read, Eq, Ord)

g1 = Ajar
g2 = Shut

prettyPrinter :: Gate -> String
-- prettyPrinter Ajar = "Ajar" -- same as default ...
-- prettyPrinter Shut = ...
prettyPrinter Ajar = "My_gate_is_open"
prettyPrinter Shut = "That_gate_is_closed"

-- show :: Gate -> String
-- show Ajar = "a"
-- show Shut = "b"

instance Show Gate where
	show g = prettyPrinter g

flipGate :: Gate -> Gate
flipGate Ajar = Shut
flipGate _ = Ajar


data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet
	deriving (Show, Read, Eq, Ord, Enum, Bounded)
-- data Color2 = TrafficRed | TrafficYellow | TrafficGreen
c1 = Orange
c2 = Blue

lc = [Red .. Blue] -- Enum
rainbow = [( minBound :: Color ) .. ( maxBound :: Color )]

-- flipList :: Int -> [Bool] -> [Bool]
-- flipList :: Int -> [Gate] -> [Gate]
-- flipList :: (Flippable a) => Int -> [a] -> [a]

flipWholeList :: (Flippable a) => [a] -> [a]
flipWholeList = map flp

class Flippable a where
	flp :: a -> a

instance Flippable Gate where
	flp = flipGate

instance Flippable Bool where
	flp = not

instance Flippable Color where
	flp Red = Violet
	flp Violet = Red 

--
-- Your exercise: complete the above example ...
--

--
-- Defining trees as recursive data type definitions
--
data OwnList a = Nil | Cons a (OwnList a)

instance (Show a) => Show (OwnList a) where
	show Nil = "<>"	
	show (Cons x xs) = (show x) ++ " ; " ++ (show xs)
	
ol1 = Cons 56 (Cons 4 (Cons 9 Nil))

ownHead :: (OwnList a) -> a
ownHead (Cons x xs) = x

sumOwnList :: (OwnList Integer) -> Integer
sumOwnList Nil = 0
sumOwnList (Cons x xs) = x + (sumOwnList xs)

data DList a b = DNil | DConsA a (DList a b) | DConsB b (DList a b) 
instance (Show a, Show b) => Show (DList a b) where
	show DNil = "[]"	
	show (DConsA x xs) = (show x) ++ "_A ; " ++ (show xs)
	show (DConsB y ys) = (show y) ++ "_B ; " ++ (show ys)
	
dl1 = DConsA 'h' (DConsA 'e' (DConsB Ajar (DConsA 'l' (DConsB Shut DNil))))

-- This is where we want to get to:
--
-- define a type to represent the expression 
--   5 + 6 * 8
data Expr 	= Val Int
		| Add Expr Expr
		| Mul Expr Expr
	-- deriving (Show)

e1 = Add (Val 1) (Mul (Val 2) (Val 3))
e2 = Mul (Val 9) (Add (Val 8) (Val 6))

instance Show Expr where
	show (Val v) = show v
	show (Add s1 s2) = (show s1) ++ "+" ++ (show s2)
	show (Mul s1 s2) = (show s1) ++ "*" ++ (show s2)
