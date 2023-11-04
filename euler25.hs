a = (3+sqrt 5)/(5+sqrt 5)
b = (1+sqrt 5)/2
c = (1-sqrt 5)/2

--calculate fibs using the geometric progression
fib n = round((a*b**n)+((1-a)*(c**n)))
total = sum (map fib [(x*3)+2 | x <- [0 .. 10]])

--bad recursive solution using the series in the pdf
fib2 1 = 2
fib2 2 = 8
fib2 n = 4*fib2(n-1) + fib2(n-2)
total2 = sum (map fib2 [1..11]) 


-- abuse lazy evaluation
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- tag each generated fib with a number
zippedFibList = zip [0..] fibs

-- abuse lazy evaluation by dropping everything but what we need,
-- then taking the head of the rest.
euler25 = head $ dropWhile (\(a,b) -> (b < 10^999)) zippedFibList
	

	
