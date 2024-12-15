from data.day15 import PUZZLE_INPUT, TEST_INPUT

sr = 0
sc = 0

top, bottom = TEST_INPUT.split("\n\n")

G = []

for r, line in enumerate(top.splitlines()):
    row = []
    for c, ch in enumerate(line):
        if ch == '@':
            sr = r
            sc = c
            ch = '.'
        row.append(ch)

    G.append(row)


r, c = sr, sc

def move(r, c, rr, cc):

    match G[r+rr][c+cc]:
        case '#':
            return (False, r+rr, c+cc)
        
        case 'O':
            return move(r+rr, c+cc, rr, cc)

        case '.':
            return (True, r+rr, c+cc)



for ch in bottom:

    rr = 0
    cc = 0

    match ch:
        case '<':
            rr, cc = 0, -1

        case '>':
            rr, cc = 0, 1

        case '^':
            rr, cc = -1, 0

        case 'v':
            rr, cc = 1, 0

    (res, r1, c1) = move(r, c, rr, cc)
    if res:
        while r1 != r or c1 != c:
            G[r1][c1] = G[r1-rr][c1-cc]
            r1 -= rr
            c1 -= cc
        r += rr
        c += cc

total = 0

for (r, row) in enumerate(G):
    for (c, ch) in enumerate(row):
        if ch == 'O':
            total += r * 100 + c

print(total)



            
                






    