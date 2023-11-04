import List (group,find)
import Maybe

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
        -- primes' = takeWhile (<= ceiling((fromIntegral n)**0.5)+1) primes
        findPrime = fromMaybe n (find evenly primes)
        
divisorCount = product . map ((1+) . length) . group . primeFactors
genTriangle n = n*(n+1) `div` 2 

testToLimit limit i
    | x >= limit = (i, genTriangle i, x)
    | otherwise = rTestToLimit
    where rTestToLimit = testToLimit limit (i+1)
          x = divisorCount i

main:: IO ()          
main = putStrLn $ show $ testToLimit 500 1