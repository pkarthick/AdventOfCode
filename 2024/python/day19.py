from collections import defaultdict
from functools import lru_cache
from data.day19 import PUZZLE_INPUT, TEST_INPUT

patterns, designs = PUZZLE_INPUT.split("\n\n")

P = defaultdict(list)
for p in patterns.split(", "):
    P[p[0]].append(p)

@lru_cache
def count_ways(design) -> int:
    count = 0
   
    if design[0] in P:
        for s in P[design[0]]:
            if s == design:
                count += 1
            elif design.startswith(s):
                count += count_ways(design[len(s):])
        
    return count

counts = [count_ways(design) for design in designs.splitlines()]

print(len(list(filter(lambda c: c > 0, counts)))) #part 1
print(sum(counts)) # part 2