package main

func main() {
	n := 4000000
	println(solution(n))
}

func solution(n int) int {
	f1 := 2
	f2 := 1
	sum := 2
	var f int
	for i := 1; f < n; i++ {
		f = f1 + f2
		f2 = f1
		f1 = f
		if f % 2 == 0 {
			sum += f
		}
	}
	return sum
}
