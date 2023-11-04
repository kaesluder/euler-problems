module Main where

import Data.Set 
import Pand
import Sieve
import qualified Data.List as L

primes = primesToQ 1000000

prime_set = fromList primes
trivial_divisors = fromList [0,2,4,5,6,8]

test_for_td n = any (\x->member x trivial_divisors) $  number_to_list n

-- need to add back in 2 and 5
primes_filtered = 2:(5:(Prelude.filter (not . test_for_td) primes))



rotate_num n = list_to_number $ x:xs
    where temp_list = number_to_list n
          x = last temp_list
          xs = init temp_list

-- rotate_list ns = x:xs
--   where temp_list = ns
--         x = last temp_list
--         xs = init temp_list

rotate_list ns = xs
    where len = length ns
          xs = drop 1 $ take (len+1) $ cycle ns


infinite_rotations n = L.unfoldr (\x -> Just (x, rotate_num x)) n

infinite_rotations_list ns = L.unfoldr (\x -> Just (x, rotate_list x)) ns

is_circular_prime n = 
    let ns = number_to_list n
        len = length ns
        rotations = L.map list_to_number $ take len $ infinite_rotations_list ns
    in all (\x-> member x prime_set) rotations
    

circular_primes = L.filter is_circular_prime $ primes_filtered 

main = putStrLn $ show $ length circular_primes    