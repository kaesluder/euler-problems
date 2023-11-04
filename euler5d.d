import std.stdio,std.math,eratosthenesd;
void main() {
	//writeln(eratoPrimes(10));
	uint euler5 = euler5product(20);
	writeln(euler5);
	//writeln(text(42));
	
}


uint logLimit(uint n, uint limit){
	return cast(uint)(log(limit)/log(n));
}

uint euler5product(uint limit){
	uint result = 1;
	foreach(uint prime; eratoPrimes(limit)){
		result *= prime^^logLimit(prime,limit);
	}
	return result;
}
