module Main where

import Modex
{-
#euler48
# The series, 1**1 + 2**2 + 3**3 + ... + 10**10 = 10405071317.
# 
# Find the last ten digits of the series, 
# 1**1 + 2**2 + 3**3 + ... + 1000**1000.

-}

prec = 10^10
euler48sum = sum $ map (\x -> expm prec x x) [1..1000]
euler48sum2 = sum $ map (\x -> (x^x) `mod` prec) [1..1000]
euler48digits = euler48sum `mod` 10^10

main = putStrLn $ show $ euler48digits









