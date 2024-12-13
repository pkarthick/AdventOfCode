import re
from data.day13 import PUZZLE_INPUT, TEST_INPUT

lines = PUZZLE_INPUT.split("\n\n")

def eq(ax, ay, bx, by, c, d):

    b = (c * ay - d * ax) // (bx * ay - by * ax)
    a = (c - (bx * b)) // ax

    return (a, b, (ax * a + (bx * b)) == c and (ay * a + by * b == d))

def solve(extra):

    total = 0

    for first, second, third in map(lambda l: l.splitlines(), lines):
        [ax, ay] = list(map(lambda s: int(s.split('+')[1]), first.split(', ')))
        [bx, by] = list(map(lambda s: int(s.split('+')[1]), second.split(', ')))
        [c, d] = list(map(lambda s: int(s.split('=')[1]), third.split(', ')))

        c += extra #comment this for part1
        d += extra #comment this for part1

        a, b, solved = eq(ax, ay, bx, by, c, d)

        if solved:
            total += a * 3 + b

    print(total)
        
solve(0) # part1
solve(10000000000000) #part 2

            




    