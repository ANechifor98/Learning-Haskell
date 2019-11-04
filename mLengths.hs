data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Enum, Show)
data Month = Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec deriving (Enum, Read)
type Date = (Int, Month, Int)

leap :: Int -> Bool
leap x
     | (x `mod` 100 == 0) = (x `mod` 400 == 0)
     | x `mod` 4 == 0     = True
     | otherwise          = False

mLengths :: Int -> [Int]
mLengths m = [31, february, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
     where february = if leap m then 29 else 28