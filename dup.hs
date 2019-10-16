--need to be able to compare the first element of the list with all of the other elements in the list
compareAllElem :: Eq a => a -> [a] -> Bool
compareAllElem _ [] = False
compareAllElem x (y:ys)  | x == y          = True
                         | otherwise       = compareAllElem x ys
--compareAllElem compares the element submitted with all of the elements that come after it
dup :: Eq a => [a] -> Bool
dup []                                     = False
dup [x]                                    = False
dup (x:xs) | compareAllElem x xs == True   = True
           | otherwise                     = dup xs