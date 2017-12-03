-- CSCE 314-599: Homework 5
-- Module: Postorder
-- Oneal Abdulrahim
-- Due: Thursday, October 19, 2017 @ 11:59p

-- Tree data type, pre-defined from the specs
data Tree a b = Branch b (Tree a b) (Tree a b) | Leaf a

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