module Main where 

import qualified Data.Numbers.Primes as P
import Data.List
import Pand
import Modex



every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []


primeCounter n x
    | P.isPrime x = n+1
    | otherwise = n

primeCounts =  tail $ zip3 (map fst gen_spiral) (tail $ scanl primeCounter 0 (map snd gen_spiral)) [1..]

primeCounts4 = every 4 primeCounts

primeCountsFilter = (\(_,b,c)-> 10*b < c)

gen_sides seed n = map (\x ->(n,(seed + (x * n * 2)))) [1..4]

gen_sides2 seed n = map (\x ->(seed + (x * n * 2))) [1..4]

gensides3 n = 
    let y = n*2+1 --convert n to side-length so the formula works
    in map (\x->(y*y) - (y*x) + x) [1..3]
diagonals n = (n*4)+1
gen_spiral3 = zip3 [1..] (prime_sides_cumulative) (map diagonals [1..])

-- prime_sides :: Integral a => a -> Integer
prime_sides n = length . filter P.isPrime $ gensides3 n
prime_sides2 n = length . filter mr_is_prime $ gensides3 n

prime_sides' n = foldl (\x acc -> if (P.isPrime x) then (acc + 1) else acc) 0 $ gensides3 n 

prime_sides_cumulative = scanl1 (+) $ map prime_sides2 [1..]




gen_many_sides xs =
    let seed = snd $ last xs
        iteration = 1 + (fst $ last xs)
    in xs ++ (gen_many_sides $ gen_sides seed iteration )

gen_spiral = gen_many_sides [(0,1)]

gen_series2 n numprimes
    | (numprimes*10 < (n*4)+1) && (numprimes > 0)  = n
    | otherwise = gen_series2 (n+1) newprimes 
        where newprimes = numprimes + (prime_sides n)
            
            --newprimes = numprimes + (length . filter P.isPrime $ gensides3 n)

mr_get_witnesses n
                 | n < 1373653 = [2,3]
                 | n < 9080191 = [31,73]
                 | n < 475912341 = [2,7,61]
                 | otherwise = [2,3,5,7,11,13,17]

mr_sd n =
     let a = n - 1
         results = map (\x-> a `div` (2^x)) [0,1..]
     in ((length . takeWhile even $ results), (head . dropWhile even $ results))

mr_is_prime_helper _ [] = True
mr_is_prime_helper n (x:xs)
                   | not (mr_test_candidate n x) = False
                   | otherwise = True && mr_is_prime_helper n xs 
mr_is_prime n =
            mr_is_prime_helper n (mr_get_witnesses n)
            


mr_test_candidate n witness
                  | n < 4 = True
                  | even n = False 
                  | otherwise = testfun_helper [0..(s-1)]
                  where  (s,d) = mr_sd n  
                         testfun r = expm n witness (d * 2^r)
                         testfun_helper [] = False
                         testfun_helper (x:xs)
                                        | ((testval == 1) && (x == 0)) 
                                                    || (testval == (n-1)) = True
                                        | otherwise = testfun_helper xs
                                        where testval = testfun x    
                                        

 

                  



e58 = 1+2*(gen_series2 1 0)
e58' = (\(a,_,_) -> 1+2*a) $ head $ filter primeCountsFilter $ gen_spiral3
main = putStrLn $ show $ e58'