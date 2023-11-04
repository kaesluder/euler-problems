module Main where 

import qualified Data.Numbers.Primes as P

nonPrimes = [y | y<-[5,7..10000], not $ P.isPrime y]
primes = 1:P.primes

isqrt :: Integral a => a -> a
isqrt = ceiling . sqrt . fromIntegral

isPrime n = (n == 1) || (P.isPrime n)

minusPrimeTwiceSquares n = 
                      let primeList = takeWhile (<= ((isqrt (n `div` 2))-1)) [1..] 
                          results = map (\x->n - (2 * x * x)) primeList
                      in results 

isGBComposite n = 
              any isPrime $ minusPrimeTwiceSquares n

euler46 = head $ filter (\x-> not $ isGBComposite x) nonPrimes

main = putStrLn $ show $ euler46




