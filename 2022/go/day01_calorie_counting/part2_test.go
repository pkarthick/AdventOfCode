package day01

import (
	"fmt"
	"os"
	"testing"
)

func TestPart2(t *testing.T) {
	Part2(sample)
	puzzle, _ := os.ReadFile(fmt.Sprintf("../../testdata/%s/puzzle_1.in", day))
	Part2(string(puzzle))
}
