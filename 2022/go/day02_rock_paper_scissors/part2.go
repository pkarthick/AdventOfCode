package day02

import (
	"fmt"
	"strings"
)

func Part2(input string) {

	lines := strings.Split(input, "\n")

	rock := 1
	paper := 2
	scissors := 3

	draw := 3
	win := 6
	loss := 0

	// A X Rock
	// B Y Paper
	// C Z Scissors

	scores := map[string]Score{
		"A X": {rock + draw, scissors + loss}, "A Y": {rock + loss, rock + draw}, "A Z": {rock + win, paper + win},
		"B X": {paper + win, rock + loss}, "B Y": {paper + draw, paper + draw}, "B Z": {paper + loss, scissors + win},
		"C X": {scissors + loss, paper + loss}, "C Y": {scissors + win, scissors + draw}, "C Z": {scissors + draw, rock + win},
	}

	mine := 0

	for _, line := range lines {
		score := scores[line]
		mine += score.mine
	}

	fmt.Println(mine)

}
