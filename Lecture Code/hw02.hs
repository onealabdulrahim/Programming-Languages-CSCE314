
{-
   myReverse:

   given a list, reverse it. (recursive)
-}

myReverse [] = []
myReverse (e:el) = (myReverse el) ++ [e]

{-
   isElement: judge if a value is an element of a list. If it is,
   return True, else return False. (recursive)
-}

isElement e [] = False
isElement e (e':el) = if (e == e') then True else (isElement e el)

{-
   duplicate:

   duplicate the elements of a list. For example, duplicate [1,2]
   returns [1,1,2,2]. (recursive)
-}

duplicate [] = []
duplicate (e:el) = [e,e] ++ (duplicate el)
-- (replicate 2 e) ++
-- (e:(e: ...)
-- !! ...

{-
   removeDuplicate:

   given a sorted list, remove the duplicated elements in this
   list. (recursive)
-}

removeDuplicate ul []     = ul
removeDuplicate ul (e:el) =
   if (elem e ul) then
    (removeDuplicate ul el)
   else
    (removeDuplicate (ul++[e]) el)

-- removeDuplicate [] [11,11,12,1,2,11,3,3,1,2]

-- idl1 = removeDuplicate (duplicate [1..10]) == [1..10]

{-
   rotate:

   rotate a list n places to the left, where n is an integer. For
   example, rotate "abcde" 2 returns "cdeab".
-}

rotate n []     = []
rotate n (e:el) = if (n > 0) then (rotate (n-1) (el ++ [e])) else (e:el)

{-
   flatten:

   flatten a list of lists into a single list formed by
   concatenation. For example, flatten [[1,2],[3,4],[5,6]] returns
   [1,2,3,4,5,6]. (recursive)
-}

flatten []     = []
flatten (el:l) = el ++ (flatten l)

{-
   isPalindrome:

   given a list, judge if it is a palindrome. 
-}

isPalindrome [ ] = True
isPalindrome [a] = True
isPalindrome  al =
  ((head al) == (last al)) && (isPalindrome (init (tail al)))

{-
   coprime:

   given two positive integer numbers, determine whether they are
   coprime. Two numbers are coprime if their greatest common divisor
   equals 1. Hint: Euclid's algorithm. (recursive)
-}

-- Euclid's GCD Algorithm

gcdE :: Int -> Int -> Int
gcdE x 0 = abs x
gcdE x y = gcdE b (mod a b)
   where a = abs x
         b = abs y

coprime x y = ((gcdE x y) == 1)

{-
   Aaah! (Easy)

   When we go to see a doctor, the doctor always asks us to say
   "aaah". Sometimes, the doctor needs us to say "aaaaaah", but we are
   only able to say "aaah". In that case, the doctor is unable to
   diagnose our disease, because the 'a's in our "aaah" are fewer than
   his or her requirements. Now, write a Haskell function called
   seeDoctor to judge if the doctor can diagnose us with our
   "aah". The input of the function consists of two strings. The first
   string is the "aaaah" the doctor needs and the second string is the
   "aah" we are able to say. Output "True" if our "aah" meets the
   requirements of the doctor, and output "False" otherwise. The test
   should pass with a "True" only when lowercase 'a's and 'h's are
   used, and each string contains a certain number of 'a's followed by
   a single 'h'.
-}

skip_letters :: Char -> [Char] -> [Char]
skip_letters l [] = [] 
skip_letters l ls =
   if ((head ls) /= l) then
      (skip_letters l (tail ls))
   else
      ls

find_aah :: [Char] -> [Char] -> [Char]
find_aah fahs []      = fahs
find_aah fahs (l:ahs) =
   if (l == 'a') then
      (find_aah (fahs ++ ['a']) ahs)
   else if ((l == 'h') && ((length fahs) > 0)) then
      fahs
   else
      []

len_as ahs = length (find_aah [] (skip_letters 'a' ahs))

seeDoctor pahs dahs = (len_as pahs) >= (len_as dahs)

{-

   Water Gates (Medium)

   There are n water gates in a reservoir that are initially
   closed. To adjust water in the reservoir, we open/close the water
   gates according to the following rule: on the first day, we open
   all the gates. Then, on the second day, we close every second
   gate. On the third day, we change the state of every third gate
   (open it if it's closed or close it if it's open) ... For the ith
   day, we change the state of every i gate. Finally, for the nth day,
   we change the state of the last gate. Our question is, how many
   gates are open after n days?

   Given n = 4. 

   At first, the gates are [closed, closed, closed, closed].

   After the first day, the gates are [open, open, open, open].

   After the second day, the gates are [open, closed, open, closed].

   After the third day, the gates are [open, closed, closed, closed]. 

   After the fourth day, the gates are [open, closed, closed, open]. 

   So you should return 2, because there are two gates open.

-}

flipGate :: Int -> [Bool] -> [Bool]
flipGate i []    = []
flipGate i gates =
   if (i /= 1) then
      ((head gates):(flipGate (i-1) (tail gates)))
   else
      ((not (head gates)):(flipGate (i-1) (tail gates)))

-- flipGate 5 (replicate 10 False)

flipGates :: [Int] -> [Bool] -> [Bool]
flipGates [] gates     = gates
flipGates (i:il) gates =
   flipGates il gates'
   where
     gates' = flipGate i gates

-- flipGates [2,4,6,8,10] (replicate 10 False)

every :: Int -> Int -> [Int]
every i n = [ x | x <- [1..n], x `mod` i == 0 ]

init_gates :: Int -> [Bool]
init_gates n = (replicate n False)

gatesNum = 4
gates    = init_gates gatesNum

gl1 = flipGates (every 1 4) gates
gl2 = flipGates (every 2 4) gl1
gl3 = flipGates (every 3 4) gl2
gl4 = flipGates (every 4 4) gl3
gli = [gl1, gl2, gl3, gl4]

waterGates :: Int -> [Bool] -> [[Bool]]
waterGates i gl = 
   if (i < (length gl)) then
      ([gl'] ++ (waterGates (i+1) gl'))
   else
      [gl']
   where gl' = (flipGates (every i (length gl)) gl)

wgl = waterGates 1 gates

test_wg = gli == wgl

-- waterGates 1 gates
