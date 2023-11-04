package main
import "fmt"

func main() {
	//fmt.Println("hello")
	fmt.Println(euler1Sum(3,5,1000))
}

func euler1Sum(x, y, limit int) (int){
	return (sumMultLimit(x,limit) +
			sumMultLimit(y,limit) - 
			sumMultLimit(x*y,limit))
}


func sumMultLimit (mult int, limit int) (int) {
	result := 0
	i := mult
	for i < limit {
		result += i
		i += mult
	}
	return result
}