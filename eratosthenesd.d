module eratosthenesd; 

import std.algorithm, std.range, std.parallelism, std.math;
import std.array;
import std.stdio;

bool[] makeSieve (uint limit){
	uint arrayLen = limit + 1;
	bool[] sieve;
	sieve.length = arrayLen;
	foreach(ref i; 2 .. arrayLen) {
		sieve[i] = true;
	}
	//writeln(sieve);
	return sieve;
}


bool[] strikeSieve (bool[] sieve, uint prime, uint limit) {
	//uint strikeMax = cast(uint)sqrt(limit*1.0) + 1;
	//writeln(strikeMax);
	for (uint i = prime * prime; i <= limit; i += prime){
		sieve[i] = false;
	}

	return sieve;

}

uint nextprime (bool[] sieve, uint lastPrime){
	//writeln(sieve[lastPrime+1..$]);
	foreach(uint i, bool isMarked; sieve[lastPrime+1..$]){
		//since i is the index of the slice, covert i
		//back to the number
		if(isMarked) {return i+lastPrime+1;}
	}
	return sieve.length -1;
}

bool[] runErato (uint limit) {
	bool[] sieve = makeSieve(limit);
	uint place = 2;
	uint strikeMax = cast(uint)sqrt(limit*1.0) + 1;
	while(place < strikeMax){
		sieve = strikeSieve(sieve,place,limit);
		place = nextprime(sieve,place);
	}
	return sieve;
}

uint[] eratoPrimes (uint limit) {
	bool[] sieve = runErato(limit);
	uint[] result;
	auto a = appender(result);
	foreach(uint i, bool isMarked; sieve){
		if (isMarked) {a.put(i);}
	}
	return a.data;

}

