package day01

import (
	"fmt"
	"strconv"
	"strings"
)

func Part1(input string) {

	maxCalories := 0
	elves := strings.Split(input, "\n\n")

	for _, calories := range elves {

		calories := strings.Split(calories, "\n")

		curr := 0

		for _, calorie := range calories {
			c, _ := strconv.Atoi(calorie)
			curr += c
		}

		if curr > maxCalories {
			maxCalories = curr
		}
	}

	fmt.Println(maxCalories)

}
