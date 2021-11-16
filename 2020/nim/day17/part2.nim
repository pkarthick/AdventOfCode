import strutils
import sequtils

let input=""".#.
..#
###"""

type

    Matrix[X, Y, Z, W: static[int]] =
        array[0..W, array[0..Z, array[0..Y, array[0..X, bool]]]]

var matrix: Matrix[20,20,20,20]

let lines = input.splitLines()
let diff = 9

for y, line in lines:
    for x, ch in line:
        matrix[diff][diff][y+diff][x+diff] = ch == '#'


proc getNeighbours(w, z, y, x: int): seq[(int,int,int,int)] =
    
    result = newSeq[(int,int,int,int)]()

    for wd in -1 .. 1:
        for zd in -1 .. 1:
            for yd in -1 .. 1:
                for xd in -1 .. 1:
                    if not (wd == 0 and zd == 0 and xd == 0 and yd == 0):
                        result.add (w+wd, z+zd, y+yd, x+xd)


proc countActiveNeighbours(w, z, y, x: int, mat: Matrix[20,20,20,20]) : bool =
    
    let delta = getNeighbours(w,z,y,x)
    let ys = delta.filterIt(it[0] > 0 and it[1] > 0 and it[2] > 0 and it[3] > 0 and it[0] <= 20 and it[1] <= 20 and it[2] <= 20 and it[3] <= 20 )
    let xs = ys.filterIt(mat[it[0]][it[1]][it[2]][it[3]])
    let l = xs.len 

    if mat[w][z][y][x]:
        if not (l == 2 or l == 3):
            return false
        # else:
        #     echo "w: ", w, " z: ", z, " y: ", y, " x: ", x

    else:
        if l == 3:
            # echo "w: ", w, " z: ", z, " y: ", y, " x: ", x
            return true

    return mat[w][z][y][x]
       

var count = 0

for round in 0 .. 5:
    
    let mat1 = matrix
    count = 0

    for w in 1..20:
        
        for z in 1..20:
    
            for y in 0..20:
                
                for x in 0..20:

                    matrix[w][z][y][x] = countActiveNeighbours(w,z,y,x, mat1)
                    
                    if matrix[w][z][y][x]:
                        inc count

            

    echo count

    
        







