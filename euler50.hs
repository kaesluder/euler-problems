module Main where 

import qualified Data.Numbers.Primes as P
import Data.List

-- finds the maximum prime sum of consecutive primes under limit and reports 
-- the prime and the length of the series used to produce it  
-- cumulative sum is produced with scanl1
maxprime xs = last . filter (\x-> P.isPrime (snd x)) $  
              zip ([1..]) (takeWhile (< 1000000) $ scanl1 (+) $ xs)

-- drop x primes from the head of the list, then 
-- return starting prime, length of series, and sum of series.
outputfun x = 
          let local_primes = drop x P.primes
              start_prime = head local_primes
              mpt = maxprime local_primes
          in (start_prime,(fst mpt),(snd mpt))  

-- map outputun to an sequence of 0-39.                   
testPrimes = map outputfun [0..40]        

-- used to determine the tuple with the maximum second term.
max_length_helper (x,y,z) (a,b,c)
                  | y > b = (x,y,z)
                  | otherwise = (a,b,c)

-- return the tuple with the maximum length
max_length = foldr1 max_length_helper $ testPrimes
                  
-- reporting to stdout for compliation 
e50 = max_length
main = putStrLn $ show $ e50