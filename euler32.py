def pand_append (al, bl, results=[]):
    for k in bl:
        cl = list(al)
        cl.append(k)
        results.append(cl)
    return results
        
#print pand_append ([],range(2,10))

def is_pandigital(n):
    nl = [c for c in str(n)]
    while len(nl) > 0:
        tval = nl.pop()
        if tval == '0':
            return False
        if tval in nl:
            return False
    else:
        return True
        
def are_pandigital(nl):
    ns = list("".join([str(x) for x in nl]))
    while len(ns) > 0:
        tval = ns.pop()
        if tval == '0':
            return False
        if tval in ns:
            return False
    else:
        return True
    
twos = [x for x in xrange(12,99) if is_pandigital(x)]
threes = [x for x in xrange(123,988) if is_pandigital(x)]
fours = [x for x in xrange(1234,9877) if is_pandigital(x)]

#set1 = [(x,y) for x in twos for y in threes if are_pandigital((x,y))]
set1 = [(x,y) for x in twos for y in threes]
set1products = [(x,y,x*y) for x,y in set1 if are_pandigital((x,y,x*y))]
#print set1products[0:5]
#set2 = [(x,y) for x in range(1,10) for y in fours if are_pandigital((x,y))]

set2 = [(x,y) for x in range(1,10) for y in fours]
set2products = [(x,y,x*y) for x,y in set2 if are_pandigital((x,y,x*y))]
#print set2products[0:5]
productset = set([x[2] for x in set1products + set2products])
print sum(productset)