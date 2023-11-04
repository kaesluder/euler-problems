import List

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

primes = primesToQ 100000

testFactor x y = x `mod` y == 0

dropFactor x y [] = []
dropFactor x y xs 
    | testFactor x y = y:foo
    | otherwise = xs
    where foo = dropFactor (div x y) y xs
    
multipleDivideBy x y
    | testFactor x y = recurseDivide
    | otherwise = x
    where recurseDivide = multipleDivideBy (div x y) y

-- lessThanPrimes :: (RealFrac a, Integral t) => a -> [t]
integerRoot n = ceiling(fromIntegral n**0.5)
lessThanPrimes n = [x | x <- primesToQ 6400000, x < n+1 ]
matchPrimes n = [x | x <- lessThanPrimes n, mod n x == 0]

factors = factors' primes
factors' _ 1      = []
factors' (x:xs) n | (n `mod` x) == 0 = (x, amount):factors' xs left
                  | otherwise        = factors' xs n
  where (amount, left) = amountLeft (0,n) x

amountLeft (amount, n) x | (n `mod` x) == 0 = amountLeft (amount + 1, n `div` x) x
                         | otherwise        = (amount, n)
                    
-- prime generator

-- primes = 2 : filter isPrime [3..]

isPrime a = isPrimeHelper a primes

isPrimeHelper a (p:ps)
        | p*p > a = True
        | a `mod` p == 0 = False
        | otherwise = isPrimeHelper a ps
        

genTriangle n = n*(n+1) `div` 2 
unfoldDrop x = unfoldr findFactor x 
    where 
        first (a,b,c) = a
        findFactor 1 = Nothing
        findFactor b = (\(_,d,p)-> Just (p, d))
            $ head $ filter ((==0).first) 
            $  map (\p -> (b `mod` p, b `div` p, p)) $ primes
            
divisorCount = product . map ((1+) . length) . group . unfoldDrop

divisorCountTriangle = divisorCount . genTriangle 

genTriangleList n = map genTriangle [1..n]
divisorCountList (xs) = map (\p -> (divisorCount p,p)) xs


divisorCountTriangleOptimized n
    | n `mod` 2 == 0 = divisorCount (n `div` 2) * divisorCount (n +1)
    | otherwise = divisorCount((n+1) `div` 2) * divisorCount n

testToLimit limit i
    | x >= limit = (i, genTriangle i, x)
    | otherwise = rTestToLimit
    where rTestToLimit = testToLimit limit (i+1)
          x = divisorCountTriangleOptimized i

testToLimit' limit i
    | x >= limit = (i, genTriangle i, x)
    | otherwise = rTestToLimit
    where rTestToLimit = testToLimit limit (i+1)
          x = divisorCountTriangle i

