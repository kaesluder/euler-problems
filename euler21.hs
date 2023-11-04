import List (group,find)
import Maybe

divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

sumDivisors = sum . divisors
range=[1..10000]
rangeDivisors = map (\x-> (x, sumDivisors x)) range
filterPair (a,b) = 
    (sumDivisors b == a) && (not (a == b))
    


amicablePairs = filter filterPair rangeDivisors
sumAmicablePairs = sum $ map (\(x,_) -> x) amicablePairs


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

primeFactors 1 = []
primeFactors n = factor : primeFactors (n `div` factor)
    where
        factor = findPrime
        evenly x = n `rem` x == 0
        -- primes' = takeWhile (<= n) primes
        findPrime = fromMaybe n (find evenly primes)
        
divisorCount = map (\x -> (head x, length x)) . group . primeFactors
sumDivisors' n = (product . map (\(a,b) -> ((a^(b+1)) - 1) `div` (a - 1)) $ divisorCount n) - n
    