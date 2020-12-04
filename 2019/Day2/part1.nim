import strutils
import sequtils

let input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,10,23,27,2,27,13,31,1,10,31,35,1,35,9,39,2,39,13,43,1,43,5,47,1,47,6,51,2,6,51,55,1,5,55,59,2,9,59,63,2,6,63,67,1,13,67,71,1,9,71,75,2,13,75,79,1,79,10,83,2,83,9,87,1,5,87,91,2,91,6,95,2,13,95,99,1,99,5,103,1,103,2,107,1,107,10,0,99,2,0,14,0"
    
var state = input.split({','}).map(parseInt)

proc evalProgram(stae: var seq[int]): int =

    state[1] = 12
    state[2] = 2

    var i = 0

    while state[i] != 99:

        let 
            loc1 = state[i+1]
            loc2 = state[i+2]
            target = state[i+3]
            op1 = state[loc1]
            op2 = state[loc2]

        case state[i]
        of 1:
            state[target] = op1 + op2 #add
        of 2:
            state[target] = op1 * op2 #multiplication
        of 99:
            break
        else:
            echo "Something is quite not right!"

        i += 4

    state[0]

echo evalProgram(state)