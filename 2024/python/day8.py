from collections import defaultdict
from itertools import combinations
from data.day08 import PUZZLE_INPUT, TEST_INPUT

INPUT = PUZZLE_INPUT

lines = INPUT.splitlines()

rowcount = len(lines)
colcount = len(lines[0])

antennas = defaultdict(list)
locations = []

for r, line in enumerate(lines):
    for c, a in enumerate(line):
        if a != '.':
            locations.append((r,c))
            antennas[a].append((r,c))

def count(limit):

    uniq = set()

    def add_items(r, c, rd, cd):

        for _ in range(limit):
            r += rd
            c += cd

            if 0 <= r < rowcount and 0 <= c < colcount:
                uniq.add((r, c))
            else:
                break

    for locs in antennas.values():
        for ((r1,c1), (r2, c2)) in combinations(locs, 2):
            add_items(r1, c1, r1-r2, c1-c2)
            add_items(r2, c2, r2-r1, c2-c1)

    return uniq


print(len(count(1)))
print(len(count(rowcount).union(locations)))
    
