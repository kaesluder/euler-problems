import numpy
limit = 1000000

results = numpy.zeros(limit,dtype=numpy.int)

print len(results)
def nextSequence(n):
    if n%2 ==0:
        return n/2
    else:
        return 3*n+1

def runSequence(n):
    i = n
    count = 1
    while i != 1:
        j = nextSequence(i)
        i=j
        count += 1
    return count

def maxSequence(limit,results):
    #results = []
    for i in xrange(1,limit):
        results[i] = runSequence(i)
    return results
    
sequenceLengths = maxSequence(limit,results)
print max(sequenceLengths)
print numpy.nonzero(sequenceLengths==525)
