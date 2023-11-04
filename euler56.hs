module Main where 

import Pand
import Data.List

sumdigits = sum . number_to_list

euler56 = maximum . map sumdigits $  [x^y|x<-[1..99],y<-[1..99]]

main = putStrLn $ show $ euler56