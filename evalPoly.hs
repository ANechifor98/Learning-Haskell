evalPoly :: Int -> [Int] -> Int
evalPoly _ [] = 0
evalPoly _ [x] = x
evalPoly n (x:xs) = x + (n * (evalPoly n xs))

--anxn + … + a2x2 + a1x + a0 = a0 + x(a1 + x(a2 + x(… an) …) )