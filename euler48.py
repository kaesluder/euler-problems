#euler48
# The series, 1**1 + 2**2 + 3**3 + ... + 10**10 = 10405071317.
# 
# Find the last ten digits of the series, 1**1 + 2**2 + 3**3 + ... + 1000**1000.

#set the precision of the results. 
prec = 10**11

#use modular exponentiation to speed things up by 
#quite a bit. Using modular exponentiation 
#is as much as 10 times faster over the whole range.

a = sum([pow(x,x,prec) for x in xrange(1,1001)])%10**10

print a
