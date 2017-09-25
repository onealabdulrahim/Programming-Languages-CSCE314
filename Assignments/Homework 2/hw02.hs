-- CSCE 314-599: Homework 2
-- Oneal Abdulrahim
-- Due: Friday, September 22, 2017 @ 11:59p

{- Resources used:
  https://stackoverflow.com
  http://learnyouahaskell.com
  https://haskell.reddit.com
  Lecture Slides
  Graham Hutton, "Programming in Haskell" (2e)
-}

{-
The 'myReverse' function reverses a given list recursively.
It takes a list as an argument and returns a type list.
-}
myReverse :: [a] -> [a]
myReverse [] = [] -- pattern match empty list
myReverse [x] = [x] -- pattern match single element (recursive base case)
myReverse ls = (ls !! (length ls - 1)) : myReverse (init ls) -- take last item then the next last...

{-
The 'isElement' judges if an element is in a list recursively.
It takes a list as an argument and returns a type bool.
-}
isElement :: Eq a => a -> [a] -> Bool
isElement x [] = False -- no elements in empty list
isElement x (n:ns) = if n == x then True else isElement x ns

{-
The 'duplicate' function duplicates every element in a list.
It takes a list as an argument and returns a type list.
-}
duplicate :: [a] -> [a]
duplicate [] = [] -- pattern match empty list (recursive base case)
duplicate (n:ns) = n : n : duplicate ns -- head of list twice, then the rest

-- $$$$$$$$$$$$$$$$$$ REMOVE DUPLICATE $$$$$$$$$$$$$$$$$$
{-
    Basic algorithm:
    1.) Sort list (ex. [1, 1, 2, 2, 3, 3, 3])
    2.) Group similar elements (ex. [[1, 1], [2, 2], [3, 3, 3]])
    3.) Take head of each element in element

-}

--Quicksort O(n log n)
quicksort :: Ord a => [a] -> [a]
quicksort [] = [] -- base case
quicksort (n:ns) = 
    let smaller = quicksort [x | x <- ns, x <= n]
        larger  = quicksort [x | x <- ns, x > n]
    in smaller ++ [n] ++ larger

-- takes a list, counts # of elements that are similar
count :: (Num a, Eq b) => b -> [b] -> a
count x [] = 0
count x (n:ns) = if n == x then 1 + count x ns else count x ns

-- group similar elements in sorted list
group :: Eq a => Int -> [a] -> [[a]]
group x [] = []
group x ls = take x ls : group (count (ls !! x) ls) (drop x ls)

{-
The 'removeDuplicate' removes all duplicated elements in a list.
It takes a list as an argument and returns a type list.
-}
removeDuplicate :: Eq a => [a] -> [a]
removeDuplicate ls = map head (group (count (head ls) ls) ls)