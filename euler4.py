from math import sqrt, ceil

def palendromesix (a, b, c):
	return (a*100001) + (b*10010) + (c * 1100)

def getfactor(number):
	a = [x for x in range(110,1000,11) if x < int(sqrt(number))]
	#list.reverse(a)
	for x in a:
		#print x
		if (number%x == 0) and (100 < number/x < 1000):
			return number, x, number/x
	return None

def factorpalendromes():	
	for a, b, c in [(x,y,z) 
			for x in range(9,0,-1) 
			for y in range(10) 
			for z in range(10)]:
		pal = palendromesix(a,b,c)
		res = getfactor(pal)
		if res: 
			print res

def reverse(n):
	reversed = 0
	while n > 0:
		reversed = 10*reversed + n%10
		n = n/10
	return reversed

def ispalendrome(n):
	return n == reverse(n)

def getpalendromes():
	a = [x for x in range(110,1000,11)]
	#list.reverse(a)
	b = range(999,100,-1)
	for y in b:
		for x in a:
			if ispalendrome(x*y):
				print x, y, x*y

factorpalendromes()	


	

	
