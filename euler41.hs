module Main where


import Sieve
import Data.List
import qualified Data.Vector as V

primes = primesToQ (10^5)
primes_vector = V.fromList primes

-- Convert a number to a list of digits. Order is 
-- reversed.
number_to_list 0 = []
number_to_list x = ntl_helper x
    where ntl_helper 0 = []
          ntl_helper x = (x `mod` 10) : ntl_helper (x `div` 10)

isqrt :: Integral a => a -> a
isqrt = ceiling . sqrt . fromIntegral

-- Convert a list of digits to a number.         
list_to_number [] = 0
list_to_number s@(x:xs) =
    x * 10^((length s)-1) + list_to_number xs

-- Recursively search for duplicates in the list.
has_duplicates [] = False
has_duplicates (x:xs)
    | (x `elem` xs) = True
    | otherwise = has_duplicates xs
    
-- Helper: Checks a list for duplicate digits 
-- and returns true if the number is pandigital.
detect_pand n = not $ has_duplicates $ number_to_list n

gen_combos_for_n x = permutations [1..x]

gen_combos = map list_to_number $ foldl1 (++) $ map gen_combos_for_n [4,7]

test_combos = V.fromList $ gen_combos

--sieve out some early primes
early_prime_sieve n = foldr1 (||) $ map (\x->n `mod` x == 0) [2,3,5]
round1 = V.filter (\x->not $ early_prime_sieve x) test_combos
round2 = V.filter (\x->not $ is_composite x) round1

is_composite n =
    let limit_primes = V.takeWhile (\x-> x < (isqrt n)) primes_vector
        test_primes = V.foldr1 (||) $ V.map (\x->n `mod` x == 0) limit_primes
    in test_primes

main = putStrLn $ show $ V.maximum round2


