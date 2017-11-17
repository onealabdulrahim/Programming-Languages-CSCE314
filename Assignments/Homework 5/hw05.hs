-- CSCE 314-599: Homework 5
-- Oneal Abdulrahim
-- Due: Thursday, October 19, 2017 @ 11:59p

{- Resources used:
  https://stackoverflow.com
  http://learnyouahaskell.com
  https://haskell.reddit.com
  Lecture Slides
  Graham Hutton, "Programming in Haskell" (2e)
-}

-- D:\OneDrive\Documents\TAMU\"Computer Science"\"CSCE 314"\Assignments\"Homework 5"\solutions

-- Tree data type, pre-defined from the specs
data Tree a b = Branch b (Tree a b) (Tree a b) | Leaf a

{-
The "spaces" function takes an integer argument & returns a String with
the given integer number of empty spaces. Used for indenting below.
@param  Int x   The number of spaces
@return [Char]  String of spaces
-}
spaces :: Int -> [Char]
spaces 0 = []
spaces x = "  " ++ spaces (x-1)

{-
Taking Tree as an instance of Show. I am not using deriving, as requested in
the specs. We define two patterns: one to display a leaf & one to display
all of the contents of a Branch, which will eventually pattern match to Leaf
-}
instance (Show a, Show b) => Show (Tree a b) where
  show = currentLevel 0
    where
      currentLevel x (Leaf a) = spaces (x*2) ++ show a
      currentLevel x (Branch b l r) = spaces (x*2) ++ show b ++ "\n"
                                   ++ currentLevel (x+1) l ++ "\n"
                                   ++ currentLevel (x+1) r ++ "\n"
                        
{-
Traverses a given tree using preorder algorithm, where the nodes are visited:
root --> left child --> right child -->
@param  x  function to map value in leaves to a common type c
        y  function to map value in leaves to a common type c
        t  Tree data type to traverse
@return [c] List of nodes in preorder
-}
preorder  :: (a -> c) -> (b -> c) -> Tree a b -> [c]
preorder x y (Leaf a)       = [x a]                                   
preorder x y (Branch b l r) = [y b] ++ preorder x y l ++ preorder x y r

{-
Traverses a given tree using postorder algorithm, where the nodes are visited:
left child --> right child --> root
@param  x  function to map value in leaves to a common type c
        y  function to map value in leaves to a common type c
        t  Tree data type to traverse
@return [c] List of nodes in postorder
-}
postorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
postorder x y (Leaf a)       = [x a]                                   
postorder x y (Branch b l r) = postorder x y l ++ postorder x y r ++ [y b]

{-
Traverses a given tree using inorder algorithm, where the nodes are visited:
left child --> root --> right child
@param  x  function to map value in leaves to a common type c
        y  function to map value in leaves to a common type c
        t  Tree data type to traverse
@return [c] List of nodes inorder
-}
inorder   :: (a -> c) -> (b -> c) -> Tree a b -> [c]
inorder x y (Leaf a)       = [x a]                                   
inorder x y (Branch b l r) = inorder x y l ++ [y b] ++ inorder x y r

{-
Provides an indication whether the given jug values can produce desired value.
@param  x    the capacity, in gallons, of the first jug of water
        y    the capacity, in gallons, of the second jug of water
        z    the desired amount of gallons
@return Bool Whether solution for Bézout's identity for given inputs exists,
             ax + by = z
<<<<<<< HEAD
=======
            
>>>>>>> origin/master
-}
measureWater :: Int -> Int -> Int -> Bool
measureWater x y z | z > x + y  = False -- impossible case, False
                   | x + y == z = True -- if we already have enough, True
                   | x == z     = True -- ""
                   | y == z     = True -- ""
                   | otherwise  = z `mod` gcd x y == 0
      -- every integer of the form ax + by is a multiple of the gcd, z