-- CSCE 314-599: Homework 5
-- Module: Inorder
-- Oneal Abdulrahim
-- Due: Thursday, October 19, 2017 @ 11:59p

-- Tree data type, pre-defined from the specs
data Tree a b = Branch b (Tree a b) (Tree a b) | Leaf a

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