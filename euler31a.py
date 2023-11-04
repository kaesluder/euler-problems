# euler31a.py

target = 200
coins = [200,2,5,10,20,50,100,1]
ways = [1]+[0]*target
 
for coin in coins:
  for i in range(coin, target+1):
    ways[i] += ways[i-coin]
  print ways
 
print "Answer to PE31 = ", ways[target]