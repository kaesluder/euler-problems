import std.stdio,numericpalendrome;


void main() {
	uint euler4 = largestPalendrome();
	writeln(euler4);
}

uint largestPalendrome() {
	uint lp = 0;
	uint a = 999;
	uint b;
	uint db;
	while (a >= 100){
		if ((a%11) == 0){
			b = 999;
			db = 1;
		} else {
			b = 990;
			db = 11;
		}
		while (b >= a) {
			if ((a*b) <= lp) {
				break;
			} if (isPalendrome(a*b)){
				lp = a*b;
			}

			b -= db;
		}
		a--;
	}

	return lp;
}