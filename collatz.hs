collatz :: Integer -> Integer

collatz n | even n    = n `div` 2
          | otherwise = 3 * n + 1

collatzSeq :: Integer -> [Integer]

collatzSeq n | n <= 0    = error "Integer must be positive"
             | n == 1    = [1]
             | otherwise = n : collatzSeq (collatz n)

maxSeq :: Integer -> Int

maxSeq n | n <= 0    = error "Integer must be positive"
         | otherwise = maximum (map (length.collatzSeq) [1..n])
         


