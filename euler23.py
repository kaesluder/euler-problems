from math import sqrt
import numpy
import cPickle



def gentriangle(x):
    return (x*(x+1))/2

def numpy_sieve(limit):
	 is_prime = numpy.ones(limit + 1, dtype=numpy.bool)
	 for n in xrange(2, int(limit**0.5 + 1.5)):
		#print n
		if is_prime[n]:
				is_prime[n*n::n] = 0
	 return numpy.nonzero(is_prime)[0][2:]		
	 
numpy_primes = numpy_sieve(1000)


def test_factor(n,p,factor_list):
    count = 0
    while True:
        if n%p == 0:
            count +=1
            n = n / p
        else:
            if count != 0:
                factor_list.append((p,count))
            break
    return n
    

def prime_factors(n):
    factor_list = []
    primes = numpy_primes
    #other_primes = [1,7,11,13,17,19,23,29]

    for p in [p for p in primes if p < (n**0.5+1)]:
        n = test_factor(n, p, factor_list)
        #print n
        #print p
        if n == 1: 
            return factor_list
    factor_list.append((n,1))
    
    
    
    return factor_list
        


def factors(n):
    factors = prime_factors(n)
    #print factors

    all = [1]
    for p,e in factors:
        prev = all[:]
        pn = 1
        for i in range(e):
            pn *= p
            all.extend([a*pn for a in prev])        
    all.sort()
    return all
            
                
    #print is_divisor
    #print numpy.nonzero(is_divisor)
    return numpy.nonzero(is_divisor)[0][0:]
    
def isAbundantNumber(x):    
    y = sum(factors(x)[:-1])
    return y > x 
    
#abundantNumbers = numpy.array([x for x in xrange(12,28123) if isAbundantNumber(x)])

#output = open('euler23data.pkl', 'wb')
#cPickle.dump(abundantNumbers,output)

input = open('euler23data.pkl', 'rb')
abundantNumbers = cPickle.load(input)

abundantNumbersSet = frozenset(abundantNumbers)

def testForPair(x):
    for y in abundantNumbers:
        if y > x: 
            return False
        elif (x-y) in abundantNumbersSet:
            return True
    return False
            

results = []
for x in xrange(1,28123):
    if not testForPair(x):
        results.append(x)
        

print sum(results)
