package Day17

import (
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

type Instruction struct {
	opcode  int64
	operand int64
}

type Program struct {
	registers    map[string]int64
	instructions []Instruction
	insPointer   int64
	halted       bool
	output       []string
}

func (prog *Program) Run() {
	for !prog.halted {
		prog.Execute()
	}
}

func (prog *Program) GetInstruction() Instruction {
	return prog.instructions[prog.insPointer]
}

func (prog *Program) NextInstruction() {
	prog.insPointer += 1
	if int(prog.insPointer) == len(prog.instructions) {
		// fmt.Println(strings.Join(prog.output, ","))
		prog.halted = true
	}
}

func (prog *Program) Execute() {

	ins := prog.GetInstruction()

	switch ins.opcode {
	case 0: //adv
		nr := prog.registers["A"]
		dr := math.Pow(2, float64((prog.comboOperand(ins.operand))))
		prog.registers["A"] = nr / int64(dr)

	case 1: //bxl
		prog.registers["B"] ^= ins.operand

	case 2: //bst
		prog.registers["B"] = prog.comboOperand(ins.operand) % 8

	case 3: //jnx
		if prog.registers["A"] != 0 {
			prog.insPointer = ins.operand
		}

	case 4: //bxc
		prog.registers["B"] ^= prog.registers["C"]

	case 5: //out
		fmt.Println("5", prog.comboOperand(ins.operand), prog.comboOperand(ins.operand)%8)
		prog.output = append(prog.output, strconv.Itoa(int(prog.comboOperand(ins.operand)%8)))
		// fmt.Println(strings.Join(prog.output, ","))

	case 6: //bdv
		nr := prog.registers["A"]
		dr := math.Pow(2, float64((prog.comboOperand(ins.operand))))
		prog.registers["B"] = nr / int64(dr)

		fmt.Println("bdv", prog.registers["A"], (prog.comboOperand(ins.operand)), dr, prog.registers["B"])

	case 7: //cdv
		nr := prog.registers["A"]
		dr := math.Pow(2, float64((prog.comboOperand(ins.operand))))
		prog.registers["C"] = nr / int64(dr)

	}

	if ins.opcode != 3 || prog.registers["A"] == 0 {
		prog.NextInstruction()
	}

}

func (prog *Program) comboOperand(operand int64) int64 {

	if operand >= 0 && operand <= 3 {
		return operand
	} else {
		switch operand {
		case 4:
			return prog.registers["A"]
		case 5:
			return prog.registers["B"]
		case 6:
			return prog.registers["C"]
		case 7:
			fmt.Println("Reserved!")
			return -1
		default:
			return -1
		}
	}
}

func (program *Program) reset(reg_a_value int64) {
	program.registers["A"] = int64(reg_a_value)
	program.registers["B"] = 0
	program.registers["C"] = 0
	program.insPointer = 0
	program.halted = false
	program.output = []string{}
}

func parseInput(file_path string) Program {
	bs, _ := os.ReadFile(file_path)
	input := string(bs)

	frags := strings.Split(input, "\n\n")
	registers_str := strings.Split(frags[0], "\n")
	registers := map[string]int64{}

	for _, register := range registers_str {
		fs := strings.FieldsFunc(register, func(r rune) bool {
			return r == ':' || r == ' '
		})

		val, _ := strconv.ParseInt(fs[2], 10, 64)
		registers[fs[1]] = val
	}

	ns := strings.Split(frags[1][9:], ",")

	instructions := []Instruction{}

	for pair := range slices.Chunk(ns, 2) {
		opcode, _ := strconv.ParseInt(pair[0], 10, 64)
		operand, _ := strconv.ParseInt(pair[1], 10, 64)

		instructions = append(instructions, Instruction{opcode, operand})
	}

	return Program{registers, instructions, 0, false, []string{}}

}

func Part1() {

	file_path := "./17/test_input1"
	program := parseInput(file_path)

	expected := "2,4,1,3,7,5,0,3,1,4,4,7,5,5,3,0"

	// nums := []int64{2, 4, 1, 3, 7, 5, 0, 3, 1, 4, 4, 7, 5, 5, 3, 0}

	expected_total := int64(2413750314475530)

	nums := []int64{5, 4, 6, 5, 2, 7, 4, 7, 6, 0, 3, 4, 5, 4, 7, 0}

	total := int64(0)

	for x := int64(777777); x > 0; x-- {

		total = 0

		e := int64(1)

		// for y := 0; y < 16; y++ {
		// 	nums[15-y] = x / e % 10
		// 	e *= 10
		// }

		for y := 3; y <= 8; y++ {
			nums[y] = x / e % 10
			e *= 10
		}

		for p, num := range nums {
			total += num * int64(math.Pow(8, float64(p+1)))

			if total > expected_total {
				break
			}

			s := strconv.Itoa(int(total))

			if strings.HasPrefix(s, "241375031") {
				fmt.Println(s, nums)
			}

		}

		if total == int64(expected_total) {
			print("Here you go!")
		}

	}

	// expected := "0,3,5,4,3,0"

	program.reset(total)
	program.Run()

	fmt.Println(program.output)

	if strings.Join(program.output, ",") == expected {
		print("Done!")

	}

}
