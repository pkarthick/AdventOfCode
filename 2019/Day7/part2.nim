import strutils
import sequtils
import algorithm

let input="3,8,1001,8,10,8,105,1,0,0,21,30,47,64,81,98,179,260,341,422,99999,3,9,1001,9,5,9,4,9,99,3,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,1001,9,5,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,1002,9,3,9,101,2,9,9,102,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99"

let input1="3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

var state = input.split({','}).map(parseInt)

proc evalProgram(state: var seq[int], i: var int, phaseSetting: int, outputSignal: var int): (int, bool) =

    while state[i] != 99:

        let 
            opcode = state[i] %% 100
            c = (state[i] div 100) %% 10
            b = (state[i] div 1000) %% 10
            # a = (state[i] div 10000) %% 10

        var
            op1 = state[i+1]
            op2 = state[i+2]

        if opcode notin @[3,4]:

            if c == 0:
                op1 = state[state[i+1]]
            if b == 0:
                op2 = state[state[i+2]]

        if state[i] > 10000:
            echo "Unexpected instruction!"
            quit 1

        case opcode
        of 1:
            state[state[i+3]] = op1 + op2 #add
            i += 4

        of 2:
            state[state[i+3]] = op1 * op2 #multiplication
            i += 4
        of 3:
            if i == 0:
                state[state[i+1]] = phaseSetting
            else:
                state[state[i+1]] = outputSignal

            i += 2

        of 4:

            if c == 0:
                outputSignal = state[state[i+1]]

            i += 2
            
            return (outputSignal, true)

        of 5:

            if op1 != 0:
                i = op2
            else:
                i += 3

        of 6:

            if op1 == 0:
                i = op2
            else:
                i += 3

        of 7:

            if op1 < op2:
                state[state[i+3]] = 1
            else:
                state[state[i+3]] = 0

            i += 4
                
        of 8:

            if op1 == op2:
                state[state[i+3]] = 1
            else:
                state[state[i+3]] = 0

            i += 4

        of 99:
            return (outputSignal, false)
        else:
            echo "Something is quite not right! Opcode is", state[i]
            break

    return (outputSignal, false)

proc findMaxSignalForThrusters(phaseSettings: seq[int]): int =

    var maxSignal = 0
    var outputSignal = 0
    var ampStates = state.repeat(5)
    var ptrs = 0.repeat(5)
    var i = 0

    while true:
        
        let (ampMaxSignal, resume) = evalProgram(ampStates[i], ptrs[i], phaseSettings[i], outputSignal)
        
        if i == 4 and ampMaxSignal > maxSignal and not resume:
            maxSignal = ampMaxSignal
            break

        i += 1
        if i == 5: i = 0
    
    return maxSignal


var phaseSettings = (5..9).toSeq
var maxThrusterSignal = 0
var next = true

while next:
    let signal = findMaxSignalForThrusters(phaseSettings)
    if signal > maxThrusterSignal:
        maxThrusterSignal = signal
    next = nextPermutation[int](phaseSettings)

echo maxThrusterSignal

