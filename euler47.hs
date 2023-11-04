module Main where 

import qualified Data.Numbers.Primes as P
import qualified Data.Set as S
import Data.List

-- most of this block isn't actually used.
primeList = takeWhile (< 200000) P.primes
primeSet = S.fromList primeList
drange delta n = [(n-delta)..(n+delta)]
rangeFour = drange 3
rangeThree = drange 1

-- filter the list of candidiates
candidates = filter (\x-> 4 == (length $ nub $ P.primeFactors x)) [1..200000]

-- a function for taking n-length slices of 
-- a sequence. I found a better way though.
takeNRecursive n xs
               | n > (length xs) = [xs]
               | otherwise = [take n xs] ++ takeNRecursive n (tail xs) 

-- toInteger is necessary to cast from length
isConsec :: [Integer] -> Bool
isConsec xs = ((last xs) - (head xs)) == ((toInteger $ length xs) - 1) 

-- this is a nice little idiom and I wish I had remembered it two hours ago.
-- if I know that the difference between n(0) and n(3) is three, I can assume 
-- the middle two are consecutive. 
euler47 = head $ filter (\(x,y)-> (y-x) == 3) $ zip candidates $ drop 3 candidates

main = putStrLn $ show $ euler47


  