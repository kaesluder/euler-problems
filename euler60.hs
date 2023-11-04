module Main where 
import Pand
import Data.Numbers.Primes
import Data.List


concatNum a b =  list_to_number $ (number_to_list a) ++ (number_to_list b)

mutualConcatPrime a b = (isPrime $ concatNum a b) && (isPrime $ concatNum b a)
allConcatPrime a xs = all (mutualConcatPrime a) xs

primeList = takeWhile (< 20000) primes
primeListTest = takeWhile (< 150) primes

createPair a b
           | a < b = (a,b)
           | otherwise = (b,a)


findMCP n candidates = [x|x<-candidates, x > n, mutualConcatPrime n x]


listFilter n x = (length x) > n




             

