package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
)

const HASH = 35
const DOT = 46
const QUESTION = 63

type Record struct {
	conditions        string
	conditions_length int
	sizes             []int
	sizes_length      int
	remaining_len     []int
	count             uint64
}

func (record *Record) count_arrangements(index int, size_index int) {

	conditions_length := len(record.conditions)

	if size_index == len(record.sizes) && index <= conditions_length {
		return
	}

	size := record.sizes[size_index]

	if index+record.remaining_len[size_index] > conditions_length {
		return
	}

	if index+1+size <= conditions_length && record.conditions[index] != '#' {
		record.count_arrangements(index+1, size_index)
	}

	if index+size > conditions_length {
		return
	}

	if index+size < conditions_length && record.conditions[index+size] == '#' {
		return
	}

	for i := range size {
		if record.conditions[index+i] == '.' {
			return
		}
	}

	if size_index == len(record.sizes)-1 {

		if index+size <= conditions_length {

			for i := index + size; i < conditions_length; i++ {
				if record.conditions[i] == '#' {
					return
				}
			}

		}

		record.count += 1

	} else {
		record.count_arrangements(index+size+1, size_index+1)
	}

}

// func newRecord(line string) Record {
// 	frags := strings.Split(line, " ")

// 	sizes_str := strings.Split(frags[1], ",")
// 	var sizes1 = make([]int, len(sizes_str))
// 	var sizes = make([]int, len(sizes_str)*5)
// 	var remaining_len = make([]int, len(sizes_str)*5)

// 	for i := range len(sizes) {
// 		size, _ := strconv.Atoi(sizes_str[i%len(sizes1)])
// 		sizes[i] = size
// 	}

// 	for i := range len(sizes) {

// 		var total = 0
// 		for j := i; j < len(sizes); j++ {
// 			total += sizes[j] + 1
// 		}

// 		remaining_len[i] = total - 1

// 	}

// 	conditions := frags[0]

// 	for i := 0; i < 4; i++ {
// 		conditions += "?" + frags[0]
// 	}

// 	return Record{conditions: conditions, conditions_length: len(conditions), sizes: sizes, sizes_length: len(sizes), remaining_len: remaining_len, count: 0}
// }

func newRecord(line string) Record {
	frags := strings.Split(line, " ")

	sizes_str := strings.Split(frags[1], ",")

	var sizes = make([]int, len(sizes_str))
	var remaining_len = make([]int, len(sizes_str))

	for i := range len(sizes) {
		size, _ := strconv.Atoi(sizes_str[i])
		sizes[i] = size
	}

	for i := range len(sizes) {
		var total = 0
		for j := i; j < len(sizes); j++ {
			total += sizes[j] + 1
		}
		remaining_len[i] = total - 1
	}

	conditions := frags[0]

	return Record{conditions: conditions, conditions_length: len(conditions), sizes: sizes, sizes_length: len(sizes), remaining_len: remaining_len, count: 0}
}

func (record *Record) addSet() {

	record.sizes = append(record.sizes, record.sizes[0:record.sizes_length]...)
	record.remaining_len = append(record.remaining_len, record.remaining_len[0:record.sizes_length]...)

	for i := range len(record.sizes) {
		var total = 0
		for j := i; j < len(record.sizes); j++ {
			total += record.sizes[j] + 1
		}
		record.remaining_len[i] = total - 1
	}

	record.conditions += "?" + record.conditions[0:record.conditions_length]
	record.count = 0

}

type Result struct {
	seq   int
	count uint64
}

func main() {
	// Open the file
	file, err := os.Open("puzzle.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()

	// Create a scanner
	scanner := bufio.NewScanner(file)

	var index = 0

	var records []Record

	// Read and print lines
	for scanner.Scan() {
		line := scanner.Text()
		record := newRecord(line)
		records = append(records, record)
		index++
	}

	fmt.Println(len(records))

	// Check for errors
	if err := scanner.Err(); err != nil {
		fmt.Println(err)
	}

	c := make(chan Result)
	var total_count uint64
	total_count = 0

	var pending = len(records)
	var wg sync.WaitGroup

	for x := 0; x < len(records); x++ {
		wg.Add(1)
		go func() {
			counts := [3]uint64{0, 0, 0}
			counts[0] = records[x].get_count()

			records[x].addSet()
			counts[1] = records[x].get_count()

			records[x].addSet()
			counts[2] = records[x].get_count()

			factor21 := counts[2] / counts[1]
			factor10 := counts[1] / counts[0]

			if factor21 == factor10 {
				c <- Result{x, counts[2] * factor21 * factor21}
			} else {
				records[x].addSet()
				records[x].addSet()

				count := records[x].get_count()
				c <- Result{x, count}
			}

			wg.Done()
		}()
	}

	for res := range c {
		pending--
		fmt.Println(res.seq, res.count, pending)
		total_count += res.count
		if pending == 0 {
			close(c)
		}
	}

	fmt.Println(total_count)

	wg.Wait()

}

func (record *Record) get_count() uint64 {
	record.count_arrangements(0, 0)
	return record.count // send sum to c
}
