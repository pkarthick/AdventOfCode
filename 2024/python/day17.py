
from dataclasses import dataclass
import itertools
from data.day17 import PUZZLE_INPUT, TEST_INPUT

@dataclass
class Instruction:
	opcode: int
	operand: int

@dataclass
class Program:
    registers:   dict[str,int]
    instructions: list[Instruction]
    insPointer:   int
    halted:       bool
    output:       list[int]

    def __init__(self, input):

        self.insPointer = 0
        self.halted = False
        self.output = []

        frags = input.split("\n\n")
        self.registers = {}

        for register in frags[0].splitlines():
            _, name, val = register.split()
            self.registers[name[:-1]] = int(val)


        ns = frags[1][9:].split(",")

        self.instructions = []

        for (opcode, operand) in itertools.batched(ns, 2):
            self.instructions.append(Instruction(int(opcode), int(operand)))

    def run(self):
        while not self.halted:
            self.execute()
            
    def has_next_instruction(self):
        self.insPointer += 1
        if int(self.insPointer) == len(self.instructions):
            self.halted = True
            return False
        return True

    def execute(self):

        ins = self.instructions[self.insPointer]

        match ins.opcode:
            case 0: #adv
                nr = self.registers["A"]
                dr = 2 ** self.combo_operand(ins.operand)
                self.registers["A"] = nr // dr

            case 1: #bxl
                self.registers["B"] ^= ins.operand

            case 2: #bst
                self.registers["B"] = self.combo_operand(ins.operand) % 8
               
            case 3: #jnx
                if self.registers["A"] != 0:
                    self.insPointer = ins.operand
                

            case 4: #bxc
                self.registers["B"] ^= self.registers["C"]

            case 5: #out
                # print("5", self.combo_operand(ins.operand), self.combo_operand(ins.operand)%8)
                self.output.append(self.combo_operand(ins.operand)%8)

            case 6: #bdv
                nr = self.registers["A"]
                dr = 2 ** self.combo_operand(ins.operand)
                self.registers["B"] = nr // dr

            case 7: #cdv
                nr = self.registers["A"]
                dr = 2 ** self.combo_operand(ins.operand)
                self.registers["C"] = nr // dr

        if ins.opcode != 3 or self.registers["A"] == 0:
            return self.has_next_instruction()
        
        return False

    def combo_operand(self, operand: int):

        if operand >= 0 and operand <= 3:
            return operand
        else:
            match operand:
                case 4:
                    return self.registers["A"]
                case 5:
                    return self.registers["B"]
                case 6:
                    return self.registers["C"]
                case 7:
                    return -1
                case _:
                    return -1
            
    def reset(self, reg_a_value: int):
        self.registers["A"] = int(reg_a_value)
        self.registers["B"] = 0
        self.registers["C"] = 0
        self.insPointer = 0
        self.halted = False
        self.output = []


    def compute_value_for_register_a(self):

        x = 7
        le = 0
        a = 7

        while True:

            for a1 in range(x * 8, x * 9):

                self.reset(a1)
                self.run()

                le = len(self.output)

                if self.output[:le] == expected[-le:]:
                    x = a * 8
                    a = a1
                    break
    
            if le == 16:
                self.registers["A"] = a
                return


# sequence
# 2 4 - b = a % 8
# 1 3 - b = b ^ 3
# 7 5 - c = a // (2 ** b)
# 0 3 - a = a ^ 8
# 1 4 - b = b ^ 4
# 4 7 - b = b ^ c (b is 2)
# 5 5 - write b mod 8 (this is 2)
# 3 0 - reset instruction pointer

expected = [2, 4, 1, 3, 7, 5, 0, 3, 1, 4, 4, 7, 5, 5, 3, 0]



def solve(part2, input):

    program = Program(input)
    
    if part2:
        program.compute_value_for_register_a()

    program.run()

    if part2:
        print(program.registers["A"])
    else:
        print(",".join(map(str, program.output)))

solve(False, TEST_INPUT)
solve(True, PUZZLE_INPUT)
