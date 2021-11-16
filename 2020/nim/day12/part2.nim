import strutils

let input="""F10
N3
F7
R90
F11"""

type 
    Direction = enum
        East = 'E'
        North = 'N'
        South = 'S'
        West = 'W'

    Coordinate = tuple[dir:Direction, dis:int]

var 
    north = 0
    east = 0

var 
    wp = ((dir:North, dis:1), (dir:East, dis:10))

proc oppositeDirection(d: Direction): Direction =
    case d
    of North: South
    of South: North
    of East: West
    of West: East

proc adjustCoordinate(c: var Coordinate, dir: Direction, dis: int) =
    
    # echo "before: ", c

    let oppdir = oppositeDirection(c.dir)

    if c.dir == dir: 
        c.dis += dis

    elif oppdir == dir: 
        if c.dis < dis:
            c.dir = oppdir
            c.dis = dis - c.dis
        else:
            c.dis -= dis

    else: discard    

    # echo "after: ", c

proc rotateLeft(c: var Coordinate, degs: int) =

    let turns = degs div 90
        
    for t in 1..turns:
        case c.dir
            of East: c.dir = North
            of North: c.dir = West
            of South: c.dir = East
            of West: c.dir = South


proc rotateRight(c: var Coordinate, degs: int) =

    let turns = degs div 90
        
    for t in 1..turns:
        case c.dir
            of East: c.dir = South
            of North: c.dir = East
            of South: c.dir = West
            of West: c.dir = North

proc computeDistances(ins: string) =

    # echo ""
    # echo ins

    let dis = ins[1..^1].parseInt

    case ins[0]
    of 'N', 'S', 'E', 'W': 
        let dir = Direction(ins[0])

        adjustCoordinate(wp[0], dir, dis)
        adjustCoordinate(wp[1], dir, dis)
        
    of 'R':
        
        rotateRight(wp[0], dis)
        rotateRight(wp[1], dis)

    of 'F':
        
        let times0 = wp[0][1] * dis

        case wp[0].dir
        of North: north += times0
        of South: north -= times0
        of East: east += times0
        of West: east -= times0

        let times1 = wp[1][1] * dis
        
        case wp[1].dir
        of North: north += times1
        of South: north -= times1
        of East: east += times1
        of West: east -= times1

    of 'L':
        rotateLeft(wp[0], dis)
        rotateLeft(wp[1], dis)

    else: 
        quit "Wrong instruction!"

    # echo "north: ", north, " east: ", east, " wp: ", wp

let instructions = input.splitLines()

for ins in instructions:
    computeDistances(ins)
            
echo abs(east) + abs(north)


