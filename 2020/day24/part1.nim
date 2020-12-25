import strutils
import tables

let input="""sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"""

proc processInstruction(s: string, n, e: var int) =

    var i = 0
    
    while i < s.len:

        if s[i] == 's' and s[i+1] == 'e':
            dec n
            # dec e
            inc i, 2
        elif s[i] == 's' and s[i+1] == 'w':
            inc e
            dec n
            inc i, 2
        elif s[i] == 'n' and s[i+1] == 'e':
            inc n
            dec e
            inc i, 2
        elif s[i] == 'n' and s[i+1] == 'w':
            inc n
            # inc e
            inc i, 2
        elif s[i] == 'e':
            dec e
            inc i
        elif s[i] == 'w':
            inc e
            inc i

let lines = input.splitLines()

var cells = initTable[(int, int), bool]()

for line in lines:
    var n, e = 0
    processInstruction(line, n, e)

    if not cells.hasKey (n,e):
        cells[(n,e)] = false
    
    cells[(n,e)] = not cells[(n,e)]

var count = 0
    
for k, b in cells:
    if b:
        inc count

echo count