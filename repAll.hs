repAll :: Eq a => a -> a -> [a] -> [a]
repAll _ _ []                 = []
repAll x y (z:zs) | x == z    = repAll x y (y:zs)
                  | otherwise = z : repAll x y zs
