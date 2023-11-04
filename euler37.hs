module Main where 

import Data.List
import Sieve
import Pand
import qualified Data.Set as S
import qualified Data.Numbers.Primes as P

primes_set = S.fromDistinctAscList $ takeWhile (< 1000000) primesTME

-- primes_set = S.fromList $ primesToQ 1000000

start_primes = [2,3,5,7]


is_prime n =
        P.isPrime n

add_digit xs =
          filter is_prime $ [(a*10)+b | a<-xs, b<-[1,3..9]] 

recursive_add_digits n xs
      | n == 0 = []
      | otherwise = xs ++ recursive_add_digits (n-1) new_digits
             where new_digits = add_digit xs  

left_trunc n
           | (n `div` 10) == 0 = 0
           | otherwise = list_to_number $ tail $ number_to_list n

left_trunc_list n = unfoldr (\b-> if b == 0 then Nothing else Just (b, left_trunc b)) n

is_left_trunc_prime n = all is_prime $ left_trunc_list n  

e37list = drop 4 . filter is_left_trunc_prime $ recursive_add_digits 6 start_primes 

e37sum = sum e37list

main = putStrLn $ show $ e37sum


selem n (x:xs)
      | x > n = False
      | x == n = True
      | otherwise = selem n xs        
