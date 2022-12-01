package day01

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

func Part2(input string) {

	maxCalories := 0
	elves := strings.Split(input, "\n\n")

	var elvesCalories []int

	for _, calories := range elves {

		calories := strings.Split(calories, "\n")

		curr := 0

		for _, calorie := range calories {
			c, _ := strconv.Atoi(calorie)
			curr += c
		}

		elvesCalories = append(elvesCalories, curr)

		if curr > maxCalories {
			maxCalories = curr
		}
	}

	sort.Ints(elvesCalories)

	l := len(elvesCalories)
	three := 0

	for _, calorie := range elvesCalories[l-3:] {
		three += calorie
	}

	fmt.Println(three)

}
