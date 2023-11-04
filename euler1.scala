object euler1 {
	def main (args: Array[String]){
		println(e1sum(3,5,1000))
	}
	
	def sumStepRange (x: Int, step: Int) = { 
		val l = List.range(step, x, step)
		l.foldLeft(0)(_ + _)}
		
	def e1sum (x: Int, y: Int, limit: Int) = {
		sumStepRange(limit, x) +
		sumStepRange(limit, y) -
		sumStepRange(limit, (x*y))
	}
		
	


}


