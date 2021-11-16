import strutils
import sets

let input="""nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""

proc signedArg(arg: string): int =
    
    case arg[0]
        of '+': result = arg[1..^1].parseInt
        of '-': result = -(arg[1..^1].parseInt)
        else: discard
        
proc execProgram(instructions: seq[string]): int =

    var acc = 0
    var cur = 0

    var executed = newSeq[int]().toHashSet

    while true:

        if executed.contains(cur):
            return acc
            
        let segs = instructions[cur].split(' ')
        let opcode = segs[0]
        let operand = segs[1].signedArg
        executed.incl cur

        case opcode
        of "acc":
            acc += operand
            cur += 1

        of "jmp":
            cur += operand

        of "nop": 
            cur += 1
            
        else: discard

echo execProgram(input.splitLines())





