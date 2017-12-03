-- CSCE 314-599: Homework 5
-- Module: Preorder
-- Oneal Abdulrahim
-- Due: Thursday, October 19, 2017 @ 11:59p

-- Tree data type, pre-defined from the specs
data Tree a b = Branch b (Tree a b) (Tree a b) | Leaf a

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