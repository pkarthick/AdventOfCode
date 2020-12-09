import strutils
import sequtils
import algorithm

let input="3,8,1001,8,10,8,105,1,0,0,21,30,47,64,81,98,179,260,341,422,99999,3,9,1001,9,5,9,4,9,99,3,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,1001,9,5,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,1002,9,3,9,101,2,9,9,102,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99"

# let input1="3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"

var state = input.split({','}).map(parseInt)

proc evalProgram(state: var seq[int], phaseSetting: int, outputSignal: var int) =

    var i = 0
    var phaseSettingMode = true

    while state[i] != 99:

        let 
            loc1 = state[i+1]
            loc2 = state[i+2]
            opcode = state[i] %% 100
            c = (state[i] div 100) %% 10
            b = (state[i] div 1000) %% 10
            # a = (state[i] div 10000) %% 10

        var
            op1 = loc1
            op2 = loc2
           

        # echo ""        
        # echo "ins:", state[i]
        # echo "opcode:", opcode
        # echo "b=", b, " c=", c
        # echo "instruction=", state[i..i+3]

        if state[i] > 10000:
            echo "Unexpected instruction!"
            quit 1

        case opcode
        of 1:
            if c == 0:
                op1 = state[loc1]
            if b == 0:
                op2 = state[loc2]

            state[state[i+3]] = op1 + op2 #add
            i += 4

        of 2:
            if c == 0:
                op1 = state[loc1]
            if b == 0:
                op2 = state[loc2]

            state[state[i+3]] = op1 * op2 #multiplication
            i += 4
        of 3:
            if phaseSettingMode:
                state[loc1] = phaseSetting
                phaseSettingMode = false
            else:
                state[loc1] = outputSignal
            i += 2

        of 4:
            if c == 0:
                outputSignal = state[loc1]
            else:
                outputSignal = loc1
            
            i += 2

        of 5:
            if c == 0:
                op1 = state[loc1]
            if b == 0:
                op2 = state[loc2]

            if op1 != 0:
                i = op2
            else:
                i += 3

        of 6:
            if c == 0:
                op1 = state[loc1]
            if b == 0:
                op2 = state[loc2]

            if op1 == 0:
                i = op2
            else:
                i += 3

        of 7:
            if c == 0:
                op1 = state[loc1]
            if b == 0:
                op2 = state[loc2]

            if op1 < op2:
                state[state[i+3]] = 1
            else:
                state[state[i+3]] = 0

            i += 4
                
        of 8:
            if c == 0:
                op1 = state[loc1]
            if b == 0:
                op2 = state[loc2]

            if op1 == op2:
                state[state[i+3]] = 1
            else:
                state[state[i+3]] = 0

            i += 4

        of 99:
            break
        else:
            echo "Something is quite not right! Opcode is", state[i]
            break

var outputSignal = 0
var phaseSettings = (0..4).toSeq
var max = 0
var next = true

while next:
    outputSignal = 0
    
    for phaseSetting in phaseSettings:
        evalProgram(state, phaseSetting, outputSignal)
        if outputSignal > max:
            max = outputSignal

    next = nextPermutation[int](phaseSettings)

echo max