module Main where 

import qualified Data.Numbers.Primes as P
import Data.List
import Pand


-- unused
primes :: [Integer]
primes =  takeWhile (< 100000) P.primes

find_dup [] = -1
find_dup (x:xs)
         | x `elem` xs = x
         | otherwise = find_dup xs

replace_two n r xs = replace_first n r $ replace_first n r xs

replace_first _ _ [] = []
replace_first n r (x:xs)
              | x == n = r:xs
              | otherwise = x:replace_first n r xs

replace_all_helper n r x
                   | x == n = r
                   | otherwise = x

replace_all n r xs = map (\x-> replace_all_helper n r x) xs

candidate_prime_filter_helper xs = 
                              let dup = find_dup xs
                                  lastx = last xs
                              in (not (dup == lastx) && (dup < 2) && (dup > -1))

candidate_primes = map list_to_number .
                   filter candidate_prime_filter_helper .
                   filter has_duplicates . 
                   map number_to_list $ P.primes


gen_new_primes n = 
               let xs = number_to_list n
                   dup_val = find_dup xs
               in filter P.isPrime . map list_to_number $ 
                  filter (\x->((head x) > 0)) $
                  map (\x->replace_all dup_val x xs) [0..9]


                   


e51 = head . filter (\x-> (length x) == 8) . map gen_new_primes $ candidate_primes

main = putStrLn $ show $ e51