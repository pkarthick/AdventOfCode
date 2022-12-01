package day01

import (
	"fmt"
	"os"
	"testing"
)

func TestPart1(t *testing.T) {
	Part1(sample)
	puzzle, _ := os.ReadFile(fmt.Sprintf("../../testdata/%s/puzzle_1.in", day))
	Part1(string(puzzle))
}
