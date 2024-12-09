from itertools import combinations
from data.day08 import PUZZLE_INPUT

INPUT = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""

INPUT = PUZZLE_INPUT

lines = INPUT.splitlines()

rowcount = len(lines)
colcount = len(lines[0])

antennas = {}
locations = set()

for r, line in enumerate(lines):
    for c, a in enumerate(line):
        if a != '.':
            locations.add((r,c))
            
            if a in antennas:
                antennas[a].append((r,c))
            else:
                antennas[a] = [(r,c)]

def count(part1):

    uniq = set()

    for a, locs in antennas.items():

        for pair in list(combinations(locs, 2)):
            ((r1,c1), (r2, c2)) = pair
            rd = r1-r2
            cd = c1-c2

            rr1 = r1
            cc1 = c1

            while True:

                rr1 += rd
                cc1 += cd

                if 0 <= rr1 < rowcount and 0 <= cc1 < colcount:
                    uniq.add((rr1, cc1))
                else:
                    break

                if part1: break


            rr2 = r2
            cc2 = c2

            while True:

                rr2 -= rd
                cc2 -= cd

                if 0 <= rr2 < rowcount and 0 <= cc2 < colcount:
                    uniq.add((rr2, cc2))
                else:
                    break

                if part1: break

    return uniq

print(len(count(True)))
print(len(count(False).union(locations)))
    
