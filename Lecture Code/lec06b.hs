--
-- Tuesday, September 28, 2017
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
-- Let us write the previous in a nicer way ...
--

import Data.Char
import Prelude hiding (return, (>>=))

type Parser a = String -> [(a, String)]
parse p inp = p inp

item :: Parser Char
item = \inp -> case inp of
		[]	-> []	
		(x:xs)	-> [(x,xs)]

ex01 = parse item "parse"

symbolColon :: Parser Char
symbolColon = \inp -> case inp of
			[]		-> 	[]
			(':':xs)	-> 	[(':',xs)]
			otherwise	-> 	[]

ex02 = parse symbolColon ":parse"

digit :: Parser Char
digit = \inp -> case inp of
		[]	-> []
		(x:xs)	-> if isDigit x then [(x,xs)] else []

ex03 = parse digit "123:parse"

char :: Char -> Parser Char
char c = \inp -> case inp of
			[]	-> 	[]
			(x:xs)	-> if x == c then [(x,xs)] else []

ex04 = parse (char 'a') "always"
ex05 = parse (char 'g') "good"
ex06 = parse (char 'g') "not good"

returnParser :: a -> Parser a
returnParser v = \inp -> [(v, inp)]

ts0 = "976-hello world"
ts1 = "hello world"

followedByValue :: Parser a -> (a -> Parser b) -> Parser b
followedByValue p1 p2 = \inp -> case (parse p1 inp) of
					[] -> []
					[(v,out)] -> parse (p2 v) out

doubleDigit = digit `followedByValue` (\d -> digit)

(>>=)  = followedByValue -- this is the bind operator
return = returnParser

doubleDigit2 = digit >>= (\d -> digit)


doubleDigit3 = 	digit >>= \d -> 
	       	digit >>= \e ->
		return [d,e]

-- Is this:
doubleDigit3' = digit >>= (\d -> (digit >>= (\e -> (return [d,e]))))


quadDigit =	doubleDigit3 >>= \dd -> 
	   	doubleDigit3 >>= \ee ->
		return (dd++ee)

phoneParser = 	doubleDigit3 >>= \prefx ->
		char '-'     >>= \foo ->
		quadDigit    >>= \suffx ->
		return (prefx ++ suffx)

pn01 = "79-5555"

ex07 = phoneParser pn01

pn = "971-555-2853"

ex08 = phoneParser pn

tripDigit = 	digit >>= \a -> 
	       	digit >>= \b ->
	       	digit >>= \c ->
		return [a,b,c]

newphoneParser = tripDigit >>= \p	->
		 char '-'  >>= \_	->
		 tripDigit >>= \q	->
		 char '-'  >>= \_	->
		 quadDigit >>= \r	->
		 return (p ++ q ++ r)
		 
pnA = "555-2853"
pnB = "55-2853"

ex09 = newphoneParser pn

-- both doubleDigit4 and tripDigit :: Parser [Char]
--newphoneParser2 = (doubleDigit4 or tripDigit) >>= p   ->
--		 char '-'  >>= \_	->
--		 quadDigit >>= \r	->
--		 return (p ++ r)
	
(+++) :: Parser a -> Parser a -> Parser a
(+++) p1 p2 = \inp -> case parse p1 inp of
			[] -> parse p2 inp
			[(v, out)] -> [(v, out)]

newphoneParser2 = (tripDigit +++ doubleDigit3 ) >>= \p   ->
		 char '-'  >>= \_	->
		 quadDigit >>= \r	->
		 return (p ++ r)

-- Look at what happens here on 555-2345
newphoneParser3 = (doubleDigit3 +++ tripDigit) >>= \p   ->
		 char '-'  >>= \_	->
		 quadDigit >>= \r	->
		 return (p ++ r)
-- if shorter doubleDigit3 is first the second choice tripDigit is skipped ...
