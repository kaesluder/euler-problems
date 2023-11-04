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
	limit = 10020
	batch = 1000
	i = 31
	while len(primes) < limit:
		#print len(primes)
		numlist = range(i,i+batch,2)
		numlist = sievewith(numlist,primes)
		numlist = sieve(numlist)
		primes = list(set(primes + numlist))
		i += batch
		
	return primes
		
	

foo = range(2,30)
bar = sieve(foo)
#print sievewith(range(30,60),bar)
baz = bigsieve()
print len(baz)
baz.sort()
print baz[999]
print baz[10000]


