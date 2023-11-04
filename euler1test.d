import std.stdio;

void main() {
	
	writeln(euler1_sum(3,5,1000));
}

int euler1_sum (int x, int y, int limit){
	//cases of x*y are added twice, so
	//subtract them from the total
	return (sum_step_limit(x,limit) +
	sum_step_limit(y,limit) -
	sum_step_limit((x*y),limit));

}

int sum_step_limit(int step, int limit){
	//iterate over the range from step to limit
	//by step.
	int i = step;
	int sum = 0;
	while (i < limit){
		sum = sum + i;
		i = i + step;
	}
	return sum;
}

