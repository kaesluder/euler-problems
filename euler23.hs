import List (group,find)
import Maybe
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Debug.Trace as Debug 


-- this is the worst port of the algorithm
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

sumDivisors = sum . divisors
range=[12..28123]

filterAbundant x = 
    sumDivisors x > x
    

abundantNumbers = filter filterAbundant range

-- vector is probably not needed here
-- used a set for lookups much faster than `elem` 
abundantNumbersVector = V.fromList abundantNumbers
abundantNumbersSet = Set.fromList abundantNumbers

isAbundant n = 
    Set.member n abundantNumbersSet
    
isSumOf n x = 
    Set.member (n-x) abundantNumbersSet
    
isSumOfPair n = 
    result
    where candidates = V.takeWhile (< n) abundantNumbersVector
          sumTest x = isSumOf n x
          -- probably a pointless attempt to optimize 
          -- would do better with a first function
          result = testForAnyInVector sumTest 0 $ candidates 
          -- result = testForAny sumTest $ V.toList candidates 
          -- result = V.dropWhile (\x -> not (sumTest x)) candidates
          
euler23 = sum $ filter (\x -> not (isSumOfPair x)) [1..28123]


-- stops processing when it gets to the fist
-- item that satisfies the list
testForAny p [] = False
testForAny p (x:xs) 
    | p x == True = True    
    | otherwise = testForAny p xs
    
testForAnyInVector p idx vec
    | V.length vec == 0 = False
    | p (vec V.! idx) = True
    | idx >= ((V.length vec)-1) = False
    | otherwise = testForAnyInVector p (idx+1) vec
        
    
    
    

  
  
    