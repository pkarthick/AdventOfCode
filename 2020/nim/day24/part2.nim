import strutils
import tables
import sets

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
    
for day in 1 .. 100:


    var blacks = initHashSet[(int, int)]()
    var whites = initHashSet[(int, int)]()

    for n in -100 .. 100:
        for e in -100 .. 100:

            var b = false

            if cells.hasKey((n, e)) and cells[(n,e)]:
                b = true

            var black = 0

            for p1 in @[(-1, 0), (-1,1), (1,-1), (1,0), (0, -1), (0, 1)]:

                let (n1, e1) = p1
                
                if cells.hasKey((n1+n, e1+e)) and cells[(n1+n, e1+e)]:
                    inc black

            if b:

                if black == 0 or black > 2:
                    whites.incl (n, e)

            else:

                if black == 2:
                    blacks.incl (n, e)


    for b in blacks:
        cells[b] = true

    for w in whites:
        cells[w] = false

    echo "Day ", day
    var count = 0
        
    for k, b in cells:
        if b:
            inc count

    echo count