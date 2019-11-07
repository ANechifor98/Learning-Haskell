data Tree a = Null | Node a (Tree a) (Tree a) deriving (Read, Show)

-- remember, base case (if a Null node is submitted) and then inductive case
-- v for value, t1 is the left subtree if value is less than the current node, t2 is the right subtree if greater
addNode :: Ord a => a -> Tree a -> Tree a 
addNode v Null = Node v Null Null
addNode v (Node n t1 t2)
     | v < n           = Node n (addNode v t1) t2
     | otherwise       = Node n t1 (addNode v t2)

-- recursive function, base case and inductive
makeTree :: Ord a => [a] -> Tree a
makeTree []        = Null
makeTree (x:xs)    = addNode x (makeTree xs)
