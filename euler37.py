import numpy

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

def list_to_number(l):
	"""Returns an int from an array of numbers."""
	return int("".join([str(c) for c in l]))
	
def number_to_list(n):
	"""Converts a number to a list. Shortest, but possibly not quickest
	algorithm."""
	return [int(i) for i in str(n)]

def is_prime(n):
    """Just a quick helper function to reduce the code below.
    Returns true if n is prime."""
    return n in numpy_primes_set


prime_list = [2,3,5,7]
add_on_list = range(1,10)

def build_nums(list1, list2):
	"""Build numbers from two lists of possible digits."""
	results = []
	for a in list1:
		for b in list2:
			c = list_to_number(number_to_list(a) + number_to_list(b))
			results.append(c)
	return results

def trunc_left(n):
	"""Check to see if a number is prime by truncating from the left."""
	nl = number_to_list(n)
	while len(nl) > 0:
		if is_prime(list_to_number(nl)):
			nl = nl[1:]
		else: return False
	return True


def repeat_build_nums(n):
	"""Iteratively build numbers from left to right, filtering for primes 
	as we go."""
	results = []
	start_set = filter(is_prime, build_nums(prime_list,add_on_list))
	results = start_set
	for x in xrange(3,n+1):
		start_set = filter(is_prime,build_nums(start_set,add_on_list))
		
		results = results + start_set
	
	#Filter the results for primes when truncated from the left as well.
	return filter(trunc_left,results)

print len(repeat_build_nums(4))

