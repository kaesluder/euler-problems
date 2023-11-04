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

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

	

	
