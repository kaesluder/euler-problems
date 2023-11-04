module euler103;


import std.algorithm, std.range, std.parallelism;
import std.array;
import std.stdio;
//import std.bigint;



//borrowed from rosetta code
T[][] combinations(T)(in T[] s, in int m) /*pure nothrow*/ {
  if (!m) return [[]];
  if (s.empty) return [];
  return s[1 .. $].combinations(m - 1).map!(x => s[0] ~ x)().array() ~
         s[1 .. $].combinations(m);
}

bool rule2(int[] a){
	//make certain that rule 2 applies to 
	//all subsets. 

	//sort the array
	sort(a);
	

	//compare i elements from the front with 
	//i - 1 elements from the back
	for (int i = 2; i <= ((a.length/2)+1); i++){
		if (sum(a[0 .. i]) <= sum(a[($ - (i - 1)) .. $])){
			return false;
		}
		
	}

	return true;

}


bool rule1Aux(int[] a, int len){
	auto subsets = a.combinations(len);
	int[][int] setHash;
	foreach(int[] subset; subsets){
		auto subsetSum = sum(subset);
    	auto hashVal = setHash.get(subsetSum, []);
    	//writeln(subset,subsetSum,hashVal);
    	if (hashVal.empty){
      		setHash[subsetSum] = subset;
      
      	} else if (!(setIntersection(hashVal,subset).empty)) {
			setHash[subsetSum] = subset ~ hashVal;
  		} else {
  			return false;
		}
	}
	//writeln(setHash[]);
	return true;
}

bool rule1(int[] a){
	//special case 1, has duplicates
	if (has_duplicates(a)) {return false;}

	//special case 2, if length < 4
	//then there are no disjoint subset to test
	//of n>1
	if (a.length < 4){
		return true;
	}

	
	//test subsets less than half of the 
	//length. Subsets greater than 1/2 length
	//can't be disjoint
	foreach(int i; 2 .. ((a.length/2)+1)){
		//writeln("foo");
		if(!(rule1Aux(a,i))){
			return false;
		}
	}



	return true;



}

int[][] get_slices(int len, int offset, int[] a){

	int[][] result;

	int limit = a.length;
	for(int i = offset; (i+len) <= limit; i = i + len){
		result ~= [(a[i .. (i+len)])];

	}
	writeln(result);
	return result;

}

bool has_duplicates (int[] a){
	//iterate over a list checking for 
	//duplicates. Uses a hash table 
	//to store seen values.
	bool[int] h;
	foreach (int i; a){
		if (h.get(i,false)){
			return true;
		}
		h[i] = true;
	}
	return false;

}




int sum(int[] a){

	return reduce!((x,y) => x + y)(0, a);
	
}

int[][] TestCases =
[[1],
[1,2],
[2,3,4],
[3,5,6,7],
[6,9,11,12,13],
[11,18,19,20,22,25],
[20,31,38,39,40,42,45],
[1,2,3,4]];



bool testBoth(int[] a){
	if (rule1(a)) {
		return (rule2(a));
	}

	return false;
}

//void main(){
//	//writeln(nearOptimum([11,18,19,20,22,25]));
//	//writeln(modularIteration(3,[0,0,0]));
//	//writeln(testBoth([11,18,19,20,22,25]));
//	//writeln(testBoth([19,30,37,38,39,41,44]));
//	writeln(euler103prep(6));
	

//}


int[] euler103prep(int delta){
	int[] e103nearOptimum = nearOptimum([11,18,19,20,22,25]);
	int[] result = modularIteration(delta,array
		(map!(x => x - (delta/2))(e103nearOptimum)));
	return result;
} 

int[] nearOptimum(int[] previous){
	int s = previous[(previous.length/2)];
	int[] b = array(map!(x => x + s)(previous));
	return [s] ~ b;

	

}

int[] modularIteration(int base, int[] arr){
	int [] result = [];
	int[] arr2;
	auto foo = new int[(base ^^ arr.length)]; 
	//foreach(int i; taskPool.parallel(foo,100)){
	for(int i = 0; i < (base^^arr.length); i++){
		arr2 = arr.dup;
		for(int j=0; j < arr.length; j++){
			arr2[j]=arr[j]+((i/base^^j)%base);
			
		}
		
		
		if (testBoth(arr2)){
			if(result.empty){result = arr2;}
			else if (sum(arr2)<sum(result)){
				result = arr2;
			}
		}		
		
	}
	
	return result;
}



