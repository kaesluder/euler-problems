import numpy

def step1(sourceList):
    """Find the largest index k such that a[k] < a[k + 1]. 
    If no such index exists, the permutation is the 
    last permutation."""
    a = None
    for k in xrange(len(sourceList)-1):
        if sourceList[k] < sourceList[k+1]:
            a = k
   
    return a


def step1a(sourceList):
    """Find the largest index k such that a[k] < a[k + 1]. 
    If no such index exists, the permutation is the 
    last permutation."""
    a = None
    
    #working backwards over the loop is a marginal
    #optimization
    for k in xrange((len(sourceList)-1),0,-1):
        if sourceList[k-1] < sourceList[k]:
            return k-1
    return a    

    
    
def step2(sourceList,a):
    """Find the largest index l such that a[k] < a[l]. 
    Since k + 1 is such an index, l is well defined and 
    satisfies k < l."""
    b = 0
    for k in xrange((len(sourceList)-1),0,-1):
        if sourceList[a] < sourceList[k]:
            return k
    return b
    
def step3(sourceList,a,b):
    """Swap a[k] with a[l]."""
    returnList = sourceList
    x = sourceList[a]
    y = sourceList[b]
    returnList[b] = x
    returnList[a] = y
    return returnList
    
def step4(sourceList,a):
    """Reverse the sequence from a[k + 1] up to and including 
    the final element a[n]."""
    returnList = sourceList
    x = returnList[:a+1]
    y = returnList[a+1:]
    y.reverse()
    returnList = x + y
    return returnList
          
        


def nextPermutation(sourceList):
    """combine all the steps to produce the next iteration
    in the cycle"""
    workingList = list(sourceList)
    a = step1a(workingList)
    
    
    #if a = None, then the list is maximally
    #sorted and there no next lex. continuation.
    
    if a == None:
        return None
    b = step2(workingList,a)
    workingList = step3(workingList,a,b)
    workingList = step4(workingList,a)
    return workingList
        
            
            
def getAllPermutations(sourceList):
    results = []
    #results.append(sourceList)
    next = list(sourceList)
    #print next
    while next != None:
        #print next
        results.append(list(next))
        next = nextPermutation(next)
        
    return results
    
def getToMillion(sourceList):
    next = list(sourceList)
    for x in xrange(1000000-1):
        next = nextPermutation(next)
    return next
    

    
            
#print nextPermutation([4,3,2,1])
print "".join([str(x) for x in getToMillion(range(10))])