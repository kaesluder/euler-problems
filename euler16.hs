-- euler16.hs

timesTwo x = 2*x
timesTwoList (xs) = map timesTwo xs

carryList [] = []
carryList (x:[])
    | x > 9 = (x `rem` 10):(x `div` 10):[]
    | otherwise = x:[]
carryList (x:y:xs) = x':carryList z'
     where x' = x `rem` 10
           y' = y + (x `div` 10)
           z' = y':xs
           
runMultiplication limit n s@(x:xs) 
    | n > limit = s
    | otherwise = runMultiplication limit n' nextIteration
        where nextIteration = carryList $ timesTwoList s
              n' = n + 1
              
testThousand = runMultiplication (1000) 1 [1]
sumDigits = sum testThousand 
main:: IO ()          
main = putStrLn $ show $ sumDigits