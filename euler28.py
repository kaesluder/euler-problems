# 
# Starting with the number 1 and moving to the right in a clockwise 
# direction a 5 by 5 spiral is formed as follows:
# 
# 21 22 23 24 25
# 20  7  8  9 10
# 19  6  1  2 11
# 18  5  4  3 12
# 17 16 15 14 13
# 
# It can be verified that the sum of the numbers on the diagonals is 101.
# 
# What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

def find_corners(n,seed=1):
    """Find the corners. Each perimiter can be divided into four segments
    of n*2. Then it's simply a matter of counting up from the value in our first cell. """
    if n == 0:
        return [seed]
    
    returnvals = [seed]
    counter = seed
    for x in xrange(1,n+1):
        for y in xrange(4):
            segment = x*2
            counter = counter + segment
            returnvals.append(counter)
        
    return returnvals
    
print sum(find_corners(500))

#based on the derivation of the sum of the four corners. 
sum_quardatic = [(4*(n*n) - 6*n + 6) for n in xrange(1,1002,2)]

print sum(sum_quardatic)
        