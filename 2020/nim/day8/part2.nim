import strutils
import sets
import options

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
        
proc modifyAndExecProgram(instructions: seq[string], modifications: var HashSet[int]): Option[int] =

    var only = false
    var acc = 0
    var cur = 0

    var executed = newSeq[int]().toHashSet

    while cur < len(instructions):

        if executed.contains(cur):
            return none(int)

        let segs = instructions[cur].split(' ')
        let opcode = segs[0]
        let operand = segs[1].signedArg

        executed.incl cur

        case opcode
        of "acc":
            acc += operand
            cur += 1

        of "jmp":
            if not only and cur notin modifications: #nop
                modifications.incl(cur)
                cur += 1
                only = true
            else:
                cur += operand

        of "nop": 
            if not only and cur notin modifications: #jmp
                modifications.incl(cur)
                cur += operand
                only = true
            else:
                cur += 1
            
        else: discard

    some(acc)

let instructions = input.splitLines()
var modifications = initHashSet[int]()

while true:

    let acc = modifyAndExecProgram(instructions, modifications)
    if acc.isSome:
        echo acc.get()
        break



