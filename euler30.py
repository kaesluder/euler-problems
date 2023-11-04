import itertools

# Euler30
# Surprisingly there are only three numbers that can 
# be written as the sum of fourth powers of their digits:
# 
#     1634 = 1**4 + 6**4 + 3**4 + 4**4
#     8208 = 8**4 + 2**4 + 0**4 + 8**4
#     9474 = 9**4 + 4**4 + 7**4 + 4**4
# 
# As 1 = 1**4 is not a sum it is not included.
# 
# The sum of these numbers is 1634 + 8208 + 9474 = 19316.
# 
# Find the sum of all the numbers that can be written as 
# the sum of fifth powers of their digits.


#create a dictionary for 
fifth_power_lookup = dict([(x, x**5) for x in xrange(0,10)])

results = []        
for i in xrange(1000,354294):
    fifth_sum = sum(fifth_power_lookup[int(x)] for x in str(i))
    #decomse to a list
    #fifth_sum_list = tuple([int(c) for c in str(fifth_sum)])
    #print i, fifth_sum_list
    if i == fifth_sum:
        results.append(fifth_sum)

print sum(results)
        

