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

-- (fromEnum month) takes the number 
-- of the index of the month in the argument from the Month data type (Jan = 0, Feb = 1 etc.)
-- step 1: add the days + the mLengths of days from that year
-- step 2: number of days from 31, December, 1752 to the date submitted by user (TAKE LEAP YEARS INTO ACCOUNT)
numDays :: Date -> Int
numDays (day, month, year) = day + sum (take (fromEnum month) (mLengths year)) + (year - 1753) * 365 + length [year | year <- [1753..year-1], leap year]