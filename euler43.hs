module Main where

import Data.List

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
detect_pand n = (not $ has_duplicates ns) 
    where ns = number_to_list n

-- finds the length of a sequence.
lengthOfNumbers xs = map (length . number_to_list) xs

divTwo = [x|x<-[102,104..986]]
testPrimes = [3,5,7,11,13,17]


appendOne xs = [(x*10)+y|x<-xs,y<-[0..9]]
appendOneAndFilter n xs = filter (\x->(((x `mod` 1000) `mod` n) == 0))$ appendOne xs

recurseApply [] (ys) = ys
recurseApply (x:xs) (ys) =
    recurseApply xs $ filter detect_pand $ appendOneAndFilter x ys

e43roots = filter detect_pand $ recurseApply testPrimes divTwo

findMisingDigit x =
    head $ [0..9] `minus` (sort $ number_to_list x)

e43pandigits = map addmissing e43roots
    where addmissing x = x + 10^9 * findMisingDigit x

minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

main = putStrLn $ show $ sum e43pandigits
