from math import factorial 

def num_to_list (n):
    results = []
    while n > 0:
        results.append(n%10)
        n = n / 10
    #results.reverse()
    return results


def list_to_num (nl):
    return int("".join([str(x) for x in nl]))
    

factorials = dict([(x,factorial(x)) for x in xrange(0,10)])

def sum_fact(n):
    f = factorials
    ntl = num_to_list
    return sum([f[x] for x in ntl(n)])

results = 0    
for x in xrange(3,2309171):
    
    if x == sum_fact(x):
        results = results + x
        print x
print results
    
