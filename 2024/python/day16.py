from data.day16 import PUZZLE_INPUT, TEST_INPUT, TEST_INPUT1

import sys
sys.setrecursionlimit(10 ** 6)

lines = TEST_INPUT1.splitlines()

sr = 0
sc = 0

er = 0
ec = 0

G = []

for r, line in enumerate(lines):
    row = []
    for c, ch in enumerate(line):
        if ch == 'S':
            sr = r
            sc = c
        if ch == 'E':
            er = r
            ec = c
        row.append(ch)
    G.append(row)

path = []

def get_valid_neighbours(triplet):

    r, c, dir = triplet

    neighbours = []

    match dir:
        case 'E':
            neighbours = [(r, c+1, 'E'), (r-1, c, 'N'), (r+1, c, 'S')]
        case 'N':
            neighbours =  [(r-1, c, 'N'), (r, c+1, 'E'), (r, c-1, 'W')]
        case 'S':
            neighbours =  [(r+1, c, 'S'), (r, c+1, 'E'), (r, c-1, 'W')]
        case 'W':
            neighbours =  [(r, c-1, 'W'), (r-1, c, 'N'), (r+1, c, 'S')]
        

    res = [(r1,c1,dir1) for (r1, c1, dir1) in neighbours if 0 < r1 < len(G)-1 and 0 < c1 < len(G[0])-1 and G[r1][c1] != '#']
    return res

minscores = {}

bestpaths = set()

pending = [(sr, sc, sr, sc)]

completed = {(sr, sc, sr, sc): (0, 'E', [])}


minscore = 10 ** 10
bestpaths = set()

while pending:

    (fr, fc, r,c) = pending.pop()

    if (fr, fc, r,c) in completed:

        (s, d, path) = completed[(fr, fc, r,c)]
      
        if r == er and c == ec:
            if s < minscore:
                bestpaths.clear()
                for p in path:
                    bestpaths.add(p)
                minscore = s
            elif s == minscore:
                minscore = s
                for p in path:
                    bestpaths.add(p)
            continue

        ns = get_valid_neighbours((r,c,d))

        for (nr, nc, nd) in ns:
        
            s1 = s + (1 if nd == d else 1001)

            path1 = path[:]
            path1.append((r, c, nr, nc))

            if (r,c,nr,nc) not in completed: 
                pending.insert(0, (r, c, nr, nc))
                completed[(r, c, nr, nc)] = (s1, nd, path1)

            elif s1 < completed[(r, c, nr, nc)][0]:
                pending.insert(0, (r, c, nr, nc))
                completed[(r, c, nr, nc)] = (s1, nd, path1)

            elif s1 == completed[(r, c, nr, nc)][0]:
                completed[(r, c, nr, nc)] = (s1, nd, completed[(r, c, nr, nc)][2] + path1)

bestpaths1 = set([(r,c) for (_,_,r,c) in bestpaths])
bestpaths1.add((sr, sc))
bestpaths1.add((er, ec))

print(minscore)
print(len(bestpaths1))
