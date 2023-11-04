#In England the currency is made up of pound, L, and 
#pence, p, and there are eight coins in general circulation:
#
#    1p, 2p, 5p, 10p, 20p, 50p, L1 (100p) and L2 (200p).
#
#It is possible to make L2 in the following way:
#
# 
#
#How many different ways can L2 be made using any number of coins?

coins = [1,2,5,10,20,50,100,200]

def nway( total, coins):
    if not coins: return 0
    c, coins = coins[0], coins[1:]
    count = 0 
    if total % c == 0: count += 1    
    for amount in xrange( 0, total, c):
        count += nway(total - amount, coins)    
    return count
# main
print nway( 200, (1,2,5,10,20,50,100,200))
