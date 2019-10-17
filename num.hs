num :: Eq a => a -> [a] -> Int
num _ []                  = 0
num x (y:ys) | x == y     = 1 + (num x ys)
             | otherwise  = num x ys