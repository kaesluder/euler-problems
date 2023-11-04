import std.algorithm, std.bigint, std.math, std.stdio, std.conv;

import numericpalendrome;


BigInt tenToNine = 10^^9;

void main() {
	
	writeln(euler104fib3(500000));
	writeln(isPandigital2(113456789));
	
}

BigInt fibToN(uint n){
	BigInt a = 1;
	BigInt b = 1;
	BigInt tmp;
	uint i = 2;
	while (i < n){
		tmp = a + b;
		a = b;
		b = tmp;
		i ++;
	}

	return b;


}

uint euler104fib(uint n){
	BigInt a = 1;
	BigInt b = 1;
	BigInt tmp;
	uint i = 2;
	while (i < n) {
		//range check
		if (b > (10^^9)){
			if (isPandigital2(lastNine(b))) {
				if (isPandigital2(firstNine(b))) {
					return i; 
				}
			}
		}
		

		tmp = a + b;
		a = b;
		b = tmp;
		i ++;
	}

	return i;


}

uint estimateFib(uint n){
	immutable double log10Phi = log10((1+sqrt(5.0))/2);
	immutable double log10Sqrt5 = log10(sqrt(5.0));
	double t = ((cast(double)n) * log10Phi) - log10Sqrt5;
	return cast(uint)(pow(10,(8+t-floor(t))));

}


uint euler104fib3(uint n){
	immutable uint chopChop = 1000000000;
	uint a = 1;
	uint b = 1;
	uint tmp;
	uint i = 2;
	while (i < n) {
		if (isPandigital(b)){
			if(isPandigital(estimateFib(i))){
				return i;
			}

		}
		tmp = (a+b)%chopChop;
		a = b;
		b = tmp;
		i++;
		
	}

	return i;


}




uint lastNine(BigInt n){
	return cast(uint)(n%tenToNine).toInt();
}

uint firstNine(BigInt n){
	return to!(uint)(text(n)[0..9]);
}


/*int firstNine(BigInt n) {
	while (n > tenToNine){
		n = n / 10;
	}
	return n;
}*/