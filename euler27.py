from math import sqrt
import numpy


# Euler published the remarkable quadratic formula:
# 
# n^2 + n + 41
# 
# It turns out that the formula will produce 40 primes for the consecutive
# values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) +
# 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is
# clearly divisible by 41.
# 
# Using computers, the incredible formula  n^2 - 79n + 1601 was discovered,
# which produces 80 primes for the consecutive values n = 0 to 79. The
# product of the coefficients, -79 and 1601, is -126479.
# 
# Considering quadratics of the form:
# 
# n^2 + an + b, where |a| < 1000 and |b| < 1000
# 
# where |n| is the modulus/absolute value of n e.g. |11| = 11 and |-4| = 4
# 
# Find the product of the coefficients, a and b, for the quadratic
# expression that produces the maximum number of primes for consecutive
# values of n, starting with n = 0.
# 
# Note to self analyze similar equations using n=0 and n=1 to figure out 
# constraints. 


def numpy_sieve(limit):
	 is_prime = numpy.ones(limit + 1, dtype=numpy.bool)
	 for n in xrange(2, int(limit**0.5 + 1.5)):
		#print n
		if is_prime[n]:
				is_prime[n*n::n] = 0
	 return numpy.nonzero(is_prime)[0][2:]		
	 
numpy_primes = numpy_sieve(1000000)

#use the x in set operation to speed up 
#prime number lookups. 
numpy_primes_set = frozenset(numpy_primes)

def find_sequence(a,b):
    """Calculate the length of the sequence from n=0
    for the equation n**n + n*a +b."""
    return_list = []
    nps = numpy_primes_set
    for n in xrange(0,b):
        c = n*n + n*a + b
        if c in nps:
            return_list.append((a,b,n,c))
        else:
            return (a,b,len(return_list))
    return (a,b,len(return_list))

def is_prime(n):
    """Just a quick helper function to reduce the code below.
    Returns true if n is prime."""
    return n in numpy_primes_set

def run_sequence(limit):
    """Returns (a,b,sequence_length) for all the candidate in
    range. b must be prime. 
    
    Need to add a special case for b == 2. But b==2 are not the droids we're looking for."""
    results = []
    set_a = [x for x in xrange((limit*-1),limit)]
    
    #set only primes
    set_b = [y for y in xrange(limit) if is_prime(y) == True]
    for b in set_b:
        for a in [x for x in set_a if abs(x) < b]:
            range1 = find_sequence(a,b)
            results.append(range1)
    return results
            

def max_sequence(result_list):
    """Collect the maximum from our results. Probably a more 
    efficient method."""
    maximum = result_list[0]
    for x in result_list:
        if x[-1] > maximum[-1]:
            maximum = x
    return maximum


#print find_sequence(1,41)
#print find_sequence(1,5)
results = max_sequence(run_sequence(1000))
print results