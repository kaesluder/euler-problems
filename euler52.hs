module Main where 

import Pand
import Data.List


test_series n = 
            let gen_series = map sort . map number_to_list $ map (* n) [1..6]
            in all (==(head gen_series)) gen_series
                   

e52 = head $ filter test_series [1..]

main = putStrLn $ show $ e52