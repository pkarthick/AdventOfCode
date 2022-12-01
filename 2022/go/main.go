package main

import (
	"bufio"
	"os"
	"strconv"
)

func main() {

}

func ReadInput() []int {
	scanner := bufio.NewScanner(os.Stdin)
	var result []int

	calorie := 0

	for scanner.Scan() {
		line := scanner.Text()

		if len(line) == 0 {
			calorie = 0
			result = append(result, calorie)
		}

		num, _ := strconv.Atoi(line)
		calorie += num

	}

	result = append(result, calorie)

	return result
}
