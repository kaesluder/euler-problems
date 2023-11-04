from math import sqrt
import numpy


def numpy_sieve(limit):
	 is_prime = numpy.ones(limit + 1, dtype=numpy.bool)
	 for n in xrange(2, int(limit**0.5 + 1.5)):
		#print n
		if is_prime[n]:
				is_prime[n*n::n] = 0
	 return numpy.nonzero(is_prime)[0][2:]		
	 
numpy_primes = numpy_sieve(1000)

#print numpy_primes[-5:]

def find_prime_period(n):
    """The periodicity of a recurring decimal is related to the 
    multiplicative order. Find the lowest x for which (10**x)%n = 1. 
    
    Best case is a period of p-1 where p is prime and 10 is a primitive 
    root of p. But I suspect that it's easier to just look for the multiplicative order.
    
    If the period isn't n-1 then it's a factor of n-1. So I use 
    (n-1)%x == 0 as a test to avoid caluculating unnecessary exponents."""
    
    #explicitly recast n as int
    #if I'm pulling from the sieve
    n = int(n)

    #limit the inner loop
    for x in xrange(1,n):
    
        #optimize by testing only factors of 
        #n-1.
        if (n-1)%x ==0:
        #if True:
            #this should be quicker than (10**x)%n
            a = pow(10,x,n)
            if a == 1:
                return n,x
            
for x in numpy_primes:
    print find_prime_period(x)


        