limit = 1000
sums = [((limit-(b+c)), b, c) for c in range(1,limit/2) for b in range(1,c)]
triangles=[(a,b,c) for (a,b,c) in sums if a**2 + b**2 == c**2 and a < b]
print triangles



