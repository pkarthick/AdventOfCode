import strutils

let input="""F10
N3
F7
R90
F11"""

let dirs = input.splitLines()

var cur = 0

var 
    north = 0
    east = 0

proc computeDistances(east, north: var int, dir: string) =

    let dis = dir[1..^1].parseInt

    case dir[0]
    of 'N': north += dis
    of 'S': north -= dis
    of 'E': east += dis
    of 'W': east -= dis
    of 'R':
        let turns = (dis %% 360) div 90
        
        for t in 1..turns:
            case cur
            of 0: cur = 2
            of 1: cur = 0
            of 2: cur = 3
            of 3: cur = 1
            else: discard

    of 'F':
        case cur
        of 0: east += dis
        of 1: north += dis
        of 2: north -= dis
        of 3: east -= dis
        else: discard

    of 'L':
        let turns = (dis %% 360) div 90
        
        for t in 1..turns:
            case cur
            of 0: cur = 1
            of 1: cur = 3
            of 2: cur = 0
            of 3: cur = 2
            else: discard


    else: 
        quit "Wrong instruction!"

    # echo "north: ", north, " east: ", east, " cur: ", allDirs[cur]

for dir in dirs:
    computeDistances(north, east, dir)
            
echo abs(east) + abs(north)


