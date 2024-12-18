from data.day18 import TEST_INPUT, PUZZLE_INPUT

import sys
sys.setrecursionlimit(10 ** 6)

INPUT = TEST_INPUT
FALL_LIMIT = 12
SIZE = 7

# INPUT = PUZZLE_INPUT
# FALL_LIMIT = 1024
# SIZE = 71

def draw(G3):

    for row in G3:
        for b in row:
            print(b, end='')

        print()

def can_reach(G, part2=False):

    pending = [(-1,-1,0,0,0)]
    minsteps = SIZE * SIZE
    visited = set([])

    while pending:

        (fr, fc, r, c, count) = pending.pop(0)

        if count > minsteps:
            continue

        if (fr,fc,r,c) in visited:
            continue

        if not (0 <= r < SIZE and 0 <= c < SIZE):
            continue

        if G[r][c] == '#':
            continue

        visited.add((fr, fc, r,c))

        if r == SIZE-1 and c == SIZE-1:
            if count < minsteps:
                minsteps = count
                # print(minsteps)
                # draw(G2)
                # print()
                # print()
                # print()

            if part2:
                return minsteps
            
        else:

            for (r1, c1) in [(r+1, c), (r, c+1), (r, c-1), (r-1, c)]:
                if r1 == fr and c1 == fc:
                    continue
                if 0 <= r < SIZE and 0 <= c < SIZE:
                    pending.append((r,c,r1,c1,count+1))

    return minsteps

def solution(part2):

    G = [['.' for _ in range(SIZE)] for _ in range(SIZE)]

    i = 0

    for line in INPUT.splitlines():
        x, y = map(int, line.split(','))
        G[y][x] = '#'

        i += 1

        if i >= FALL_LIMIT:

            steps = can_reach(G, part2)
 
            if part2 and steps == SIZE * SIZE:
                print(line)
                break

            if not part2:
                print(steps)
                break
   
solution(False)
solution(True)