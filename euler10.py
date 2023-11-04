import itertools
import numpy

def eliminate(n, numlist):
	return [x for x in numlist if (x%n !=0) or (x==n)]

def sieve(numlist):
	a = numlist[0]
	b = numlist[-1]
	while (a**2 < b):
		numlist = eliminate(a,numlist)
		a = numlist[numlist.index(a)+1]
		#print a
	return numlist

def sievewith(numlist,primelist):
	for a in primelist:
		if a**2 < max(numlist):
			numlist = eliminate(a, numlist)
	return numlist
	
def bigsieve():
	primes = sieve(range(2,30))
	#print primes
	limit = 100000
	i = 3
	bigarray = range(1,limit,2)
	while i < bigarray[-1]:
		#bigarray = itertools.ifilter((lambda x: x%i !=0),bigarray)
		#print i
		for j in xrange(i,((bigarray[-1])/i+1)):
 			product = i*j
 			#print product
 			if product in bigarray:
 				bigarray.pop(bigarray.index(product))
 		i = bigarray[bigarray.index(i)+1]
		
		
	return bigarray

def erat_sieve(bound):
	if bound < 2:
		return []
	max_ndx = (bound - 1) / 2
	sieve = [True] * (max_ndx + 1)
	#loop up to square root
	for ndx in range(int(bound ** 0.5) / 2):
		print ndx
		# check for prime
		if sieve[ndx]:
			# unmark all odd multiples of the prime
			num = ndx * 2 + 3
			sieve[ndx+num:max_ndx:num] = [False] * ((max_ndx-ndx-num-1)//num + 1)
	# translate into numbers
	return [2] + [ndx * 2 + 3 for ndx in range(max_ndx) if sieve[ndx]]

def numpy_sieve(limit):
	 is_prime = numpy.ones(limit + 1, dtype=numpy.bool)
	 for n in xrange(2, int(limit**0.5 + 1.5)):
		#print n
		if is_prime[n]:
				is_prime[n*n::n] = 0
	 return numpy.nonzero(is_prime)[0][2:]		
		
		
	

#foo = range(2,30)
#bar = sieve(foo)
#print sievewith(range(30,60),bar)
foo = erat_sieve(200)
print sum(foo)
numpy_sieve(200)

