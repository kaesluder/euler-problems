module sudoku;
import std.stdio, std.algorithm, std.range, std.string, std.conv;

struct Cell {
	uint fixedValue = 0;
	bool[uint] _possibleValues;

	uint row;
	uint col;
	uint id;

	/** A function that initializes the 
	possibleValues array. */

	void setup(uint x, uint y) {
		bool[uint] result;
		foreach (i; iota(1,10)){
			result [i] = true;

		}
		_possibleValues = result;
		row = x;
		col = y;
		id = (10 * x) + y;
	}


	/** return possible values as an array.
	*/

	auto possible() {
		uint[] result;
		foreach (i; iota(1,10)){
			if (_possibleValues[i])
				result ~= i;

		}
		return result;
	}

	auto prune(uint i) {
		_possibleValues[i] = false;
		return possible();
	}

	auto pruneList(uint[] li){
		foreach (i; li)
			_possibleValues[i] = false;

		return possible();
	}

	/** If we fix the value, then there are no possible values. */
	auto set(uint i){
		fixedValue = i;
		_possibleValues = [0u:false];
	}

	//** test for dead ends */
	bool deadEndTest(){
		if (fixedValue != 0)
			return false;
		foreach (b;_possibleValues.values)
			if (b)
				return false;

		//default return;

		return true; 
	}


}

struct Puzzle {
	Cell[9][9] cellArray;

	void setup(){
		foreach (i; iota(0,9))
			foreach (j; iota(0,9))
				cellArray[i][j].setup(i,j);
	}

	Cell[] getRow(uint i){
		return cellArray[i];
	}

	auto getColumn(uint j){
		return cellArray[][j];
	}

	auto getBlock(uint x, uint y){
		Cell[] results;
		uint starti = x * 3;
		uint startj = y * 3;
		foreach (i; iota(starti,starti+3))
			foreach (j; iota(startj,startj+3))
				results ~= cellArray[i][j];
		return results;
	}

	auto getFixedValues(Cell[] cells){
		uint[] results;
		foreach (cell; cells)
			if (cell.fixedValue != 0)
				results ~= cell.fixedValue;
		return results;


	}

	bool detectDuplicates(Cell[] cells){
		auto nums = getFixedValues(cells);
		bool[uint] numhash;
		foreach (i; nums){
			if (numhash.get(i,false)){
				return true;
			} else {
				numhash[i] = true;
			}
		}

		return false;
	}

	/**Test for duplicate fixed values in the 
	puzzle. */


	bool testDuplicates(){

		//test rows and columns
		foreach (i; iota(0,9)){
			if (detectDuplicates(getRow(i)) ||
				detectDuplicates(getColumn(i)))
				return true;

		}

		//test blocks
		foreach (i;iota(0,3))
			foreach (j; iota(0,3)){
				if (detectDuplicates(getBlock(i,j)))
					return true;
			}
		//default return
		return false;
	}

	/** Test for dead ends. Elements with no fixed value 
	and no possibile values. */

	bool testDeadEnds(){
		foreach (i; iota(0,9))
			foreach(j; iota(0,9))
				if (cellArray[i][j].deadEndTest())
					return true;
		return false;
	}


	string prettyPrintLine(uint i){
		string result;
		foreach (j,cell; getRow(i)){
			//writeln(cell.fixedValue);
			if (j % 3 == 0){
				//writeln(j);
				result ~= " ";
				result ~= text(cell.fixedValue);

			} else {
				result ~= text(cell.fixedValue);
			}

		}

		//writeln(result);
		return result;
	}


	void prettyPrintPuzzle(){
		foreach(i,line;cellArray[]){
			if (i % 3 == 0){
				writeln();
				writeln(prettyPrintLine(i));
			} else {
				writeln(prettyPrintLine(i));
			}
		}
	}



}

