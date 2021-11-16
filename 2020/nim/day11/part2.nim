import strutils
import sequtils

let input="""L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""

var rows = input.splitLines()

proc modifySeats(): bool =

    let copy = rows.mapIt(it).toSeq

    proc checkRow(r, c: int): int =
        var count = 0

        for i in countdown(c-1, 0):
            if copy[r][i] == '#':
                inc count
                break
            elif copy[r][i] == 'L':
                break

        for i in c+1..len(copy[0])-1:
            if copy[r][i] == '#':
                inc count
                break
            elif copy[r][i] == 'L':
                break

        return count

    proc checkColumn(r, c: int): int =
        var count = 0

        for i in countdown(r-1, 0):
            if copy[i][c] == '#':
                inc count
                break
            elif copy[i][c] == 'L':
                break

        for i in r+1..len(copy)-1:
            if copy[i][c] == '#':
                inc count
                break
            elif copy[i][c] == 'L':
                break

        return count

    proc checkDiagonals(r, c: int): int =

        var count = 0

        var ri = r
        var ci = c

        while true:
            dec ri
            dec ci
            if ri < 0 or ci < 0: 
                break
            
            if copy[ri][ci] == '#':
                inc count
                break
            elif copy[ri][ci] == 'L':
                break

        ri = r
        ci = c

        while true:
            inc ri
            inc ci
            if ri == len(copy) or ci == len(copy[0]): 
                break
            
            if copy[ri][ci] == '#':
                inc count
                break
            elif copy[ri][ci] == 'L':
                break

        ri = r
        ci = c

        while true:
            dec ri
            inc ci
            if ri < 0 or ci == len(copy[0]): 
                break
            
            if copy[ri][ci] == '#':
                inc count
                break
            elif copy[ri][ci] == 'L':
                break

        ri = r
        ci = c

        while true:
            inc ri
            dec ci
            if ri == len(copy) or ci < 0: 
                break
            
            if copy[ri][ci] == '#':
                inc count
                break
            elif copy[ri][ci] == 'L':
                break

        return count

    proc checkAllDirections(r,c: int) : int =
        checkRow(r,c) + checkColumn(r,c) + checkDiagonals(r,c)

    var changed = false
    for r, row in rows:
        for c, seat in row:
            
            if seat == 'L' and checkAllDirections(r,c) == 0:
                    rows[r][c] = '#'
                    changed = true
            
            elif seat == '#' and checkAllDirections(r,c) >= 5:
                    rows[r][c] = 'L'
                    changed = true

    changed 

while modifySeats():
    discard

echo rows.mapIt(it.filterIt(it == '#').len).foldl(a+b)
