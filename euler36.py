def num_to_binary(n):
	"""Converts a number to a list representing the number in base 2."""
	if n == 0:
		return 0
	x = n
	results = []
	while x > 0:
		#print x
		if x%2 == 0:
			results.append(0)
		else:
			results.append(1)
		x = x/2
	results.reverse()
	return results
	

def fun_reverse(l):
	"""Functional version of reverse. Returns a new reversed list."""
	m = list(l)
	m.reverse()
	return m
	
def number_to_list(n):
	"""Converts a number to a list. Shortest, but possibly not quickest
	algorithm."""
	return [int(i) for i in str(n)]
	
def is_palendrome(l):
	"""Test if a list is palendrome."""
	if l == fun_reverse(l):
		return True
	else: return False
	
def is_binary_palendrome(n):
	"""Test if a number is a palendrome in base 2."""
	return is_palendrome(num_to_binary(n))
	
def list_to_number(l):
	"""Returns an int from an array of numbers."""
	return int("".join([str(c) for c in l]))

def gen_palendromes(n):
	"""Returns a set of two palendromes constructed from an integer. 
	If n has digits xyz function returns xyzzyx and xyzyx."""
	k = number_to_list(n)
	l = fun_reverse(k)
	return (list_to_number(k+l),list_to_number(k+l[1:]))
	
def gen_palendrome_list(l):
	"""Returns all the palendromes that can be constructed using a list
	of numbers. """
	results = []
	for n in l:
		a, b = gen_palendromes(n)
		results.append(a)
		results.append(b)
	return results
	

#print gen_palendrome_list(xrange(1,1000))

euler36 = sum([x for x in gen_palendrome_list(xrange(1,1000)) 
				if is_binary_palendrome(x)])
print euler36