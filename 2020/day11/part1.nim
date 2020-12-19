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

    proc check(r, c: int, chars: seq[char], offlimit: bool): bool =

        if r >= len(copy) or r < 0: return offlimit
        if c >= len(copy[0]) or c < 0: return offlimit
        
        copy[r][c] in chars

    var changed = false
    for r, row in rows:
        for c, seat in row:
            let surrounding = [(r, c-1), (r, c+1), (r+1, c), (r-1, c), (r+1,c-1), (r+1, c+1), (r-1, c-1), (r-1, c+1)]

            if seat == 'L' and surrounding.allIt(check(it[0], it[1], @['L', '.'], true)):
                rows[r][c] = '#'
                changed = true
            
            elif seat == '#' and surrounding.filterIt(check(it[0], it[1], @['#'], false)).len >= 4:
                rows[r][c] = 'L'
                changed = true

    changed 

while modifySeats():
    discard

echo rows.mapIt(it.filterIt(it == '#').len).foldl(a+b)


