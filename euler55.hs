module Main where 

import Pand
import Data.List


reverseNum n = list_to_number $ number_to_listr n
reverseAdd n = n + reverseNum n

runSequence n count 
            | count > 50 = (-1)
            | n == reversed = count
            | otherwise = runSequence (n + reversed) (count + 1)
              where reversed = list_to_number $ number_to_listr n

-- start with one iteration to catch symmetric 
-- lychrel numbers.
startSequence n = runSequence (reverseAdd n) 1




e55 = length . filter (== (-1)) $ map startSequence [1..10000]

main = putStrLn $ show $ e55