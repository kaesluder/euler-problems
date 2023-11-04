package main 

import (
	"fmt"
	"sort"
	"math"
	
)

func main() {
	//fmt.Println(
	//	fibToN(329468))
	//fmt.Println(isPandigital(1234567891))
	fmt.Println(euler104fib(500000))
}

func fibToN(n uint32) uint32 {
	const chopchop = 1000000000
	var i, a, b, tmp uint32 = 2, 1, 1, 0
	for i < n {
		tmp = a + b
		a = b
		b = tmp % chopchop
		i = i + 1

	}
	return b
}


func euler104fib(n uint32) uint32 {
	const chopchop = 1000000000
	var i, a, b, tmp uint32 = 2, 1, 1, 0
	for i < n {
		if isPandigital(b) {
			if isPandigital(estimateFib(i)) {
				return i
			}

		}

		tmp = a + b
		a = b
		b = tmp % chopchop
		i = i + 1

	}
	return i
}


func numberToRangeReversed(n uint32) []int {
	p := make([]int, 0)
	for n >= 10 {
		p = append(p, int(n%10))
		n = n/10
	}
	return append(p,int(n))
}

func rangeCmp(p []int, q []int) bool{
	if len(p) != len (q) {
		return false
	}

	for i := 0; i < len (p); i++{
		if p[i] != q[i] {
			return false
		}
	}

	return true
}

func estimateFib(n uint32) uint32 {
	log10Phi := math.Log10(math.Phi)
	log10Sqrt5 := math.Log10(math.Sqrt(5.0))
	t := (float64(n) * log10Phi) - log10Sqrt5
	return uint32(math.Pow(10,(8+t-math.Floor(t))))	

	
}

func isPandigital(n uint32) bool {
	if n < 123456789 {return false }
	if n > 987654321 {return false }
	p := numberToRangeReversed(n)
	sort.Ints(p)
	if p[0] == 0 {
		return false
	}
	q := []int{1,2,3,4,5,6,7,8,9}
	return rangeCmp(p, q)

}