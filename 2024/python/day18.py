from data.day18 import TEST_INPUT, PUZZLE_INPUT

import sys
sys.setrecursionlimit(10 ** 6)

INPUT = TEST_INPUT
FALL_LIMIT = 12
SIZE = 7

INPUT = PUZZLE_INPUT
FALL_LIMIT = 1024
SIZE = 71

def draw(G3):

    for row in G3:
        for b in row:
            print(b, end='')

        print()

def count_steps(G, part2=False):

    pending = [(-1,-1,0,0)]
    minsteps = SIZE * SIZE
    steps = {(0,0): 0}

    while pending:

        (fr, fc, r, c) = pending.pop(0)

        count = steps[(r, c)]

        if r == SIZE-1 and c == SIZE-1:
            if part2:
                return count
            elif count < minsteps:
                minsteps = count
                # print(minsteps)
                # draw(G2)
                # print()
                # print()
                # print()
            
        else:

            for (r1, c1) in [(r+1, c), (r, c+1), (r, c-1), (r-1, c)]:
                if (r1 != fr or c1 != fc) and 0 <= r1 < SIZE and 0 <= c1< SIZE and G[r1][c1] != '#':
                    
                    existing_steps = steps.get((r1,c1), -1)
                    
                    if existing_steps == -1 or count+1 < existing_steps:
                            steps[(r1,c1)] = count + 1
                            pending.append((r,c,r1,c1))

    return minsteps

def solution(part2):

    G = [['.' for _ in range(SIZE)] for _ in range(SIZE)]

    for i, line in enumerate(INPUT.splitlines(), start=1):
        x, y = map(int, line.split(','))
        G[y][x] = '#'

        if i >= FALL_LIMIT:

            steps = count_steps(G, part2)

            if part2:
                if steps == SIZE * SIZE:
                    print(line)
                    break
            else:
                
                print(steps)
                break

solution(False)
solution(True)