delAll :: Eq a => a -> [a] -> [a]
delAll _ []                  = []
delAll x (y:ys) | x == y     = delAll x ys               
                | otherwise  = y : delAll x ys