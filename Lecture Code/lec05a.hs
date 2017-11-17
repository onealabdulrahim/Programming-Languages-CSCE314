--
-- Tuesday, September 26, 2017
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
-- Exercise: Convert Goldbach's weak conjecture from C to Haskell
--           This gives an answer to the second Goldbach question in HW02
-- 
-- Link:
--    http://www.mathblog.dk/project-euler-46-odd-number-prime-square/
--
{-

private bool isTwiceSquare(long number) {
    double squareTest = Math.Sqrt(number/2);
    return squareTest == ((int)squareTest);
}

-- and the main loop looks like

int[] primeList = ESieve(10000);
int result = 1;
bool notFound = true;
 
while(notFound){
    result += 2;
 
    int j = 0;
    notFound = false;
    while (result >= primeList[j]) {
        if(isTwiceSquare(result-primeList[j])){
            notFound = true;
            break;
        }
        j++;
    }
}

-}

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

data Gate = Ajar | Shut
	deriving (Show, Read, Eq, Ord)

data DList a b = DNil | DConsA a (DList a b) | DConsB b (DList a b) 
instance (Show a, Show b) => Show (DList a b) where
	show DNil = "[]"	
	show (DConsA x xs) = (show x) ++ "_A ; " ++ (show xs)
	show (DConsB y ys) = (show y) ++ "_B ; " ++ (show ys)
	
dl1 = DConsA 'h' (DConsA 'e' (DConsB Ajar (DConsA 'l' (DConsB Shut DNil))))

-- This is what we want to get to:
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

--
-- Now on to looking at solutions to HW02 ... next time parsers ...
--