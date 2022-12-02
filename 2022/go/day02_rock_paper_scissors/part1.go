package day02

import (
	"fmt"
	"strings"
)

type Score struct {
	opponent int
	mine     int
}

func Part1(input string) {

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
		"A X": {rock + draw, rock + draw}, "A Y": {rock + loss, paper + win}, "A Z": {rock + win, scissors + loss},
		"B X": {paper + win, rock + loss}, "B Y": {paper + draw, paper + draw}, "B Z": {paper + loss, scissors + win},
		"C X": {scissors + loss, rock + win}, "C Y": {scissors + win, paper + loss}, "C Z": {scissors + draw, scissors + draw},
	}

	mine := 0

	for _, line := range lines {
		score := scores[line]
		mine += score.mine
	}

	fmt.Println(mine)

}
