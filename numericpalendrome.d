module numericpalendrome;
import std.array, std.algorithm, std.stdio;

uint[] numberToArrayReversed(uint n){
	//algorithm returns a reversed array
	//of single-digit integers.
	//uint[] result;
	auto a = appender!(uint[]);
	uint i = n;
	while (i >= 10){
		a.put(i%10);
		i = i / 10;

	} 
	a.put(i);
	return a.data;
	
}

uint[] numberToArray(uint n){
	uint[] result = numberToArrayReversed(n);
	reverse(result);
	return result;
}

bool isPalendrome(uint n) {
	uint[] a = numberToArrayReversed(n);
	uint[] b = a.dup;
	reverse(b);
	//writeln(a,b);
	return equal(a,b);
}

bool isPandigital(uint n){
	//bounds checking
	if (n < 123456789) {return false;}
	if (n > 987654321) {return false;}
	auto a = numberToArrayReversed(n);
	sort(a);
	if (a[0] == 0) {return false;}
	return ((findAdjacent(a).length) == 0);

}

bool isPandigital2(uint n){
	//bounds checking
	if (n < 123456789) {return false;}
	if (n > 987654321) {return false;}
	bool[] checklist = new bool[10];
	uint[] a = numberToArrayReversed(n);
	foreach (uint i; a) {
		if ((i == 0) || checklist[i] ){
			return false;
		}
		checklist[i] = true;
	}

	return true;
	

}