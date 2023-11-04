module Euler27 where

import qualified Data.Set as S

{-
# Euler published the remarkable quadratic formula:
# 
# n^2 + n + 41
# 
# It turns out that the formula will produce 40 primes for the consecutive
# values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) +
# 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is
# clearly divisible by 41.
# 
# Using computers, the incredible formula  n^2 - 79n + 1601 was discovered,
# which produces 80 primes for the consecutive values n = 0 to 79. The
# product of the coefficients, -79 and 1601, is -126479.
# 
# Considering quadratics of the form:
# 
# n^2 + an + b, where |a| < 1000 and |b| < 1000
# 
# where |n| is the modulus/absolute value of n e.g. |11| = 11 and |-4| = 4
# 
# Find the product of the coefficients, a and b, for the quadratic
# expression that produces the maximum number of primes for consecutive
# values of n, starting with n = 0.
# 
# Note to self analyze similar equations using n=0 and n=1 to figure out 
# constraints. 
-}

-- euler12.hs
primesToQ m = 2 : sieve [3,5..m]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p..m])
    
-- ordered lists, difference and union
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

primes = primesToQ 10000

prime_set = S.fromList primes

quadratic a b n = n*n + a*n + b

buildlist n = [(x,y) | x<-[(-1)*n..n],y<-primes, (abs x) < y, y <=n]


length_primes a b =
    let seq = [0..b]
        seq_len = length . takeWhile (==True) $ 
            map (\x -> S.member (quadratic a b x) prime_set) seq
    in (a,b,seq_len)

full_set = map (\(a,b) -> length_primes a b) $ buildlist 1000 

tuple_three_max (a,b,c) (x,y,z)
    | c > z = (a,b,c)
    | otherwise = (x,y,z)
    
euler27 = foldr1 tuple_three_max full_set
main = putStrLn $ show $ euler27

    

