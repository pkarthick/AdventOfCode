package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

type Pair struct {
	first  byte
	second byte
}

type PairCounter struct {
	counter  map[byte]uint64
	extended []byte
}

func getCount(s []byte) map[byte]uint64 {
	result := make(map[byte]uint64)
	for i := 0; i < len(s); i++ {
		result[s[i]] += 1
	}
	return result
}

func readInput() (string, map[Pair]byte) {
	scanner := bufio.NewScanner(os.Stdin)
	template := ""

	if scanner.Scan() {
		template = scanner.Text()
		if scanner.Scan() {
			_ = scanner.Text()
		}
	}

	rules := make(map[Pair]byte)

	for scanner.Scan() {
		line := scanner.Text()
		pair := Pair{line[0], line[1]}
		rules[pair] = line[6]
	}

	return template, rules
}

func extendRules(rules map[Pair]byte, times int) map[Pair]PairCounter {
	counter := make(map[Pair]PairCounter)

	for pair, _ := range rules {
		counter[pair] = extendPair(pair, times, rules)
	}

	return counter
}

func extendPair(pair Pair, times int, rules map[Pair]byte) PairCounter {
	polymers := []byte{pair.first, pair.second}

	for i := 0; i < times; i++ {

		temp := []byte{polymers[0]}

		for j := 0; j <= len(polymers)-2; j++ {
			mid, _ := rules[Pair{polymers[j], polymers[j+1]}]
			temp = append(temp, mid, polymers[j+1])
		}

		polymers = temp

	}
	return PairCounter{getCount(polymers), polymers}
}

func extendTemplate(template string, counter map[Pair]PairCounter) map[byte]uint64 {
	finalCounter := make(map[byte]uint64)

	for i := 0; i < len(template)-1; i++ {
		ctr := counter[Pair{template[i], template[i+1]}]
		for j := 0; j < len(ctr.extended)-1; j++ {
			ctr1 := counter[Pair{ctr.extended[j], ctr.extended[j+1]}]
			for k, ct := range ctr1.counter {
				finalCounter[k] += ct
			}
			finalCounter[ctr.extended[j+1]] -= 1
		}
	}

	finalCounter[template[len(template)-1]] += 1
	return finalCounter
}

func minMax(finalCounter map[byte]uint64) (uint64, uint64) {

	min := uint64(math.MaxUint64)
	max := uint64(0)

	for _, v := range finalCounter {
		if v < min {
			min = v
		} else if v > max {
			max = v
		}
	}

	return min, max
}

func main() {

	template, rules := readInput()
	counter := extendRules(rules, 5)
	finalCounter := extendTemplate(template, counter)
	min, max := minMax(finalCounter)

	fmt.Println(max - min)
}
