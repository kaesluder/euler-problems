-- test function for leap years. 
-- only the first condition satisfied triggers
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0 = True
    | otherwise = False

-- length of months. December is omited because we get the 
-- number for Jan 1 from the total days in the year. 
nonLeapYear = [31,28,31,30,31,30,31,31,30,31,30]
leapYear = [31,29,31,30,31,30,31,31,30,31,30]

totalDaysInYear year
    | isLeapYear year = 366
    | otherwise = 365

-- Create a list of (year, offset) pairs 
-- Jan 1, 1901 is a Tuesday so we start with an offset of 2.
-- Other offsets are created by adding the number of days in the year. 
offsetNumber = scanl addSecond (1901,2) $ map (\x->(x,totalDaysInYear (x-1))) [1902..2000]

-- Helper fuction for offsetNumber above.
addSecond (a,b) (x,y) = (x,b+y)

-- Add the offset to the days of the month. 
addDays year n
    | isLeapYear year = scanl (+) n leapYear
    | otherwise = scanl (+) n nonLeapYear

-- map addDays to offsetNumber 
-- useful for troubleshooting
monthsMap = map (\(x,y) -> (x, addDays x y)) offsetNumber

-- filter out Sundays (everything evenly divisible by 0) 
-- and dump the years
sundayFilter = map (\(x,y) -> [z | z<-y, z `mod` 7 == 0]) monthsMap

-- get the total number of sundays.
totalSundays = sum $ map length sundayFilter

    

    