-- euler 33
-- fractions reducible by this method are of the form
-- (10*n+i)/(10*i+d) = n/d
-- i > d > n

-- create a formula for both sides of the equality
leftSide n d i = d*(10*n + i)
rightSide n d i = n*(10*i+d)

-- test for equality 
isReduction n d i = (leftSide n d i) == (rightSide n d i) 

-- get all the fractions using a list comprehension. 
euler33list1 = [(n,d,i)| i <- [3..9], d<-[2..(i-1)], n<-[1..(d-1)], isReduction n d i]

-- helper functions to expand the fraction 
expandedNumerator n i = 10*n+i
expandedDenominator d i = 10*i+d

-- list 2 is the expanded fractions.
euler33list2 = map (\(n, d, i)->((expandedNumerator n i),(expandedDenominator d i))) $ euler33list1

-- a function to find the product of 
-- both pairs of the tuple
productFractions fractionList = ((product $ map fst fractionList),(product $ map snd fractionList))

-- reduce a single fraction tuple by finding the 
-- greatest common denominator
reduceFractionTuple (x,y) = (x',y')
    where gcdxy = gcd x y
          x' = x `div` gcdxy
          y' = y `div` gcdxy

-- our final result
euler33 = reduceFractionTuple $ productFractions $ euler33list2
