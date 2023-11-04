from math import sqrt

a = (3+sqrt(5))/(5+sqrt(5))
b = (1+sqrt(5))/2
c = (1-sqrt(5))/2

#calculate fibs using the geometric progression
def fib (n):
	return (a*b**n)+((1-a)*(c**n))
	
#range estimated from log(4000000)/log(1.6)

fibs = [int(round(fib(x))) for x in range(1,33)]
even_fibs = [x for x in fibs if x%2 == 0]


print fibs
print even_fibs
print sum(even_fibs)
print [pow(1.618,x) for x in range (1,33)]



	
