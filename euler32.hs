module Main where

import Data.Traversable
import Data.List

make_lists xs n = sequenceA $ replicate n xs


-- Convert a number to a list of digits. Order is 
-- reversed.
number_to_list 0 = []
number_to_list x = ntl_helper x
    where ntl_helper 0 = []
          ntl_helper x = (x `mod` 10) : ntl_helper (x `div` 10)


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
detect_pand n = not $ has_duplicates $ number_to_list n

-- Detects mutual pandigital numbers by converting 
-- both to a list, concatinating, and looking for 
-- duplicates.
detect_mutual_pand x y =
    let xl = number_to_list x
        yl = number_to_list y
    in not . has_duplicates $ xl ++ yl

-- Three argument version of the above.    
detect_mutual_pand3 x y z =
    let xl = number_to_list x
        yl = number_to_list y
        zl = number_to_list z
    in not . has_duplicates $ xl ++ yl ++ zl

-- Helper: returns true if the number doesn't 
-- contain the number 0.
nozero x = notElem 0 $ number_to_list x


-- A simple map function to yield pairs and their products.
tuple_products xs = map (\(x,y) -> (x,y,x*y)) xs

-- Filter a list to return only pandigital triplets.
pand_products_filter xs = 
    filter (\(x,y,z) -> detect_mutual_pand3 x y z) $
    filter (\(_,_,z) -> nozero z) xs

-- Collect all two-, three-, and four-digit pandigitals    
pand_twos = [x|x<-[11..98], detect_pand x, nozero x]
pand_threes = [x|x<-[123..987], detect_pand x,nozero x]
pand_fours = [x|x<-[1234..9876], detect_pand x, nozero x]

-- Build, then filter the products. 
pand23 = [(x,y) | x<-pand_twos, y<-pand_threes, detect_mutual_pand x y]
pand23products = tuple_products pand23
pand23products_filtered = pand_products_filter pand23products
    
pand14 = [(x,y) | x<-[2..9], y<-pand_fours, detect_mutual_pand x y]
pand14products = tuple_products pand14
pand14products_filtered = pand_products_filter pand14products 

-- Remove duplicate products.
euler32 =
    nubBy (\(_,_,x) (_,_,y) -> x == y) $
    pand23products_filtered ++ pand14products_filtered
    
-- Produce the sum.
euler32sum = sum $ map (\(_,_,x) -> x) euler32

main = putStrLn $ show $ euler32sum
        
    