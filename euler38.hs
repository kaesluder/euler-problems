module Main where

e38SeriesProducts x n = map (* x) [1..n]

-- Convert a number to a list of digits. Order is 
-- reversed.
number_to_list 0 = []
number_to_list x = reverse $ ntl_helper x
    where ntl_helper 0 = []
          ntl_helper x = (x `mod` 10) : ntl_helper (x `div` 10)

-- Helper: returns true if the number doesn't
-- contain the number 0.
nozero x = notElem 0 $ number_to_list x

-- Convert a list of digits to a number.         
list_to_number [] = 0
list_to_number s@(x:xs) =
    x * 10^((length s)-1) + list_to_number xs

-- Recursively search for duplicates in the list.
has_duplicates [] = False
has_duplicates (x:xs)
    | x `elem` xs = True
    | otherwise = has_duplicates xs
    
-- Helper: Checks a list for duplicate digits 
-- and returns true if the number is pandigital.
detect_pand n = (not $ has_duplicates ns) && (notElem 0 ns)
    where ns = number_to_list n

-- finds the length of a sequence.
lengthOfNumbers xs = map (length . number_to_list) xs

concatenateNumber xs = list_to_number $ concat $ map number_to_list xs
buildRange = [(x,2)|x<-[500..10000]] ++ [(x,3)|x<-[100..333]] ++ [(x,4)|x<-[25..34]] ++ [(x,5)|x<-[5..9]]

productsOverRange = map (\(x,n)->(x,n,(concatenateNumber $ e38SeriesProducts x n))) buildRange

fproducts = filter (\(_,_,x)-> detect_pand x) productsOverRange

euler38 = maximum $ map (\(_,_,x)->x) fproducts

main = putStrLn $ show $ euler38

