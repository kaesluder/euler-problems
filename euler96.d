import std.stdio, std.algorithm, sudoku, std.range,	
	std.file, std.string, std.ascii, std.array;

int main(char[][] args) {
	
	auto bar = Puzzle();
	bar.setup();
	bar.cellArray[0][0].set(1);
	bar.cellArray[0][1].set(2);
	bar.cellArray[0][2].set(8);
	auto baz = bar.getBlock(0,0);
	writeln(bar.testDeadEnds());
	auto testCaseLines = splitPuzzles(fileLines("sudoku.txt"))[0];
	auto testCase = parsePuzzle(testCaseLines);
	testCase.prettyPrintPuzzle();


	return 0;
}


//input  function
//get the lines from a file
string[] fileLines(string filename){
	string raw = cast(string) read(filename);
	return splitLines(raw);
}


string[][] splitPuzzles(string[] lines){
	string[][] results;
	auto limit = lines.length;
	uint numberOfPuzzles = limit/10;
	foreach(i;iota(0,numberOfPuzzles))
		results ~= lines[(i*10)..((i+1)*10)];
	return results;
}

Puzzle parsePuzzle(string[] lines){
	auto result = Puzzle();
	result.setup();
	foreach(i;iota(0,9))
		foreach(j;iota(0,9)){
			uint val = (cast(uint) lines[i+1][j]) - 48;
			if (val != 0) {
				result.cellArray[i][j].set(val);
			}
		}
	return result;

}


