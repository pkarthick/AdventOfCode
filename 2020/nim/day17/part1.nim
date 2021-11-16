import strutils
import sequtils

let input=""".#.
..#
###"""

type

    Matrix[X, Y, Z: static[int]] =
        array[0..X, array[0..Y, array[0..Z, bool]]]

var matrix: Matrix[20,20,20]

let lines = input.splitLines()

let diff = 9

for y, line in lines[0 .. ^1]:
    for x, ch in line:
        matrix[diff][y+diff][x+diff] = ch == '#'

proc getNeighbours(z, y, x: int): seq[(int,int,int)] =
    
    result = newSeq[(int,int,int)]()

    for zd in -1 .. 1:
        for yd in -1 .. 1:
            for xd in -1 .. 1:
                if not (zd == 0 and xd == 0 and yd == 0):
                    result.add (z+zd, y+yd, x+xd)

proc countActiveNeighbours(z, y, x: int, mat: Matrix[20,20,20]) : bool =
    
    let delta = getNeighbours(z,y,x)
    
    let l = delta.filterIt(it[0] > 0 and it[1] > 0 and it[2] > 0 and it[0] <= 20 and it[1] <= 20 and it[2] <= 20 ).filterIt(mat[it[0]][it[1]][it[2]]).len 

    if mat[z][y][x]:
        if not (l == 2 or l == 3):
            return false

    else:
        if l == 3:
            return true

    return mat[z][y][x]


for round in 0 .. 5:

    var count = 0
    let mat = matrix

    for z in 0..20:
        for y in 0..20:
            for x in 0..20:
                matrix[z][y][x] = countActiveNeighbours(z,y,x, mat)
                
                if matrix[z][y][x]:
                    inc count

    echo count






