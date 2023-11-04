import std.stdio;

long euler2sum(long limit){
	long f1 = 1;
	long f2 = 2;
	long sum = 0;

	while (f2 < limit){
		if ((f2%2) == 0) { sum += f2; }
		long temp = f1 + f2;
		f1 = f2;
		f2 = temp;



	}

	return sum;
}

void main() {
	writeln(euler2sum(4_000_000));
	
}