let rec sum_aux limit n sum i =
  if (i >= limit) then sum
  else (sum_aux limit n (sum+i) (i+n));;
  
let sum_limit_mult limit n =
	sum_aux limit n 0 0;;
	
let euler1_sum limit x y =
	(sum_limit_mult limit x) +
	(sum_limit_mult limit y) -
	(sum_limit_mult limit (x * y));;
	
let euler1 = euler1_sum 1000 3 5;;
