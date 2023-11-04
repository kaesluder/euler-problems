import std.algorithm, std.stdio, std.conv, std.array, std.string,std.parallelism;
import euler103;

int[][] readSets () {

	string[] results;
	auto app = appender(results);
	auto f = File ("sets.txt","r");
	foreach (string line; lines(f)){
		app.put(line);
		//writeln(line);
	}

	auto chomped = (map!lineParser(app.data)).array();
	
	//parallel
	//auto chomped = taskPool.amap!lineParser(app.data);

	return chomped;


}

int[] lineParser(string line){
	string[] splitted = split(chomp(line),",");
	//writeln(map!(to!int)(splitted));
	return (map!(to!int)(splitted)).array();

}

int testSum(int[] s){
	if (testBoth(s)) {
		return sum(s);
	}
	return 0;
}

int[] testCase(){
	int[][] sets = readSets;
	int[][] t = sets[0 .. 2];
	//writeln(t);
	return map!(testSum)(t).array();
	//return [];
}

int[] euler105test(){
	int[][] sets = readSets;

	//not parallel
	int[] sums = (map!testSum(sets)).array();
	
	//parallel
	//int[] sums = taskPool.amap!testSum(sets,5);
	
	//writeln(sums);
	return sums;
}

int euler105(){
	return sum(euler105test());
}

void main (){

	//writeln(readSets());
	//writeln(lineParser("123,456\n"));
	//writeln(testBoth([1,2,3,4]));
	writeln(euler105());
}

