from collections import defaultdict
from data.day23 import TEST_INPUT, PUZZLE_INPUT

INPUT = PUZZLE_INPUT

lines =  INPUT.splitlines()

conns = defaultdict(set)

computers = set()
t_computers = set()

for line in lines:
    c1, c2 = line.split('-')
    conns[c1].add(c2)
    conns[c2].add(c1)
    computers.add(c1)
    computers.add(c2)

    if c1[0] == 't':
        t_computers.add(c1)

    if c2[0] == 't':
        t_computers.add(c2)

threes = set()
connected_computers = set()


for tc in t_computers:
    for c1 in conns[tc]:
        for c2 in conns[c1]:
            if c1 != tc and c2 != tc and c2 in conns[tc]:
                threes.add(tuple(sorted((tc, c1, c2))))
                connected_computers.add(tc)
                connected_computers.add(c1)
                connected_computers.add(c2)

print(len(threes)) #part1

accs = set()

for k, cs in conns.items():
    acc = set([k])

    for c in cs:
        for s in acc:
            if c not in conns[s]:
                break
        else:
            acc.add(c)

    accs.add(tuple(sorted(acc)))


final = sorted(accs, key=len, reverse=True)

print(",".join(final[0])) # part2








