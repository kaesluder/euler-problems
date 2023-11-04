module Main where 

import qualified Data.Numbers.Primes as P
import qualified Data.Set as S
import Data.List
import Pand

primes =  dropWhile (< 1000) $ takeWhile (< 10000) P.primes

testForSequence n =
                let perms = sort $ nub $ map list_to_number 
                            $ permutations $ number_to_list n
                    filtered = filter P.isPrime  
                               $ filter (\x-> (x < 10000) && (x > 1000)) perms
                    triples = [[x,y,z]|x<-filtered,y<-filtered,z<-filtered,x<y,y<z]
                    filtered2 = filter equidistant triples       
                in filtered2


-- improves on the above by eliminating numbers from the 
-- list of candidates
testForSequence' [] results = results
testForSequence' candidates results = 
                 let perms = sort . nub . map list_to_number 
                            . permutations . number_to_list $ head candidates
                     filtered = filter (\x-> (x > 1000) && (P.isPrime x)) perms
                     new_candidates = candidates \\ filtered
                     triples = [[x,y,z]|x<-filtered,y<-filtered,z<-filtered,
                               x<y,y<z,((z-y)==(y-x))]
                     new_results = results ++ triples
                 in testForSequence' new_candidates new_results
                     


differences xs = zipWith (-) xs $ tail xs

equidistant xs = 
            let ys = differences xs 
            in all (\x-> x == (head ys)) ys

e49 = testForSequence' primes []

main = putStrLn $ show $ e49