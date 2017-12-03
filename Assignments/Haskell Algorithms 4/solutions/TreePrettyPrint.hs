-- CSCE 314-599: Homework 5
-- Oneal Abdulrahim
-- Due: Thursday, October 19, 2017 @ 11:59p

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