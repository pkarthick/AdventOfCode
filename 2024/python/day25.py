from data.day25 import TEST_INPUT, PUZZLE_INPUT

INPUT = PUZZLE_INPUT.split('\n\n')

locks = []
keys = []

def getheights(schematic):

    trimmed = schematic[1:-1]
    heights = []

    for c in range(len(trimmed[0])):
        height = 0
        for r, row in enumerate(trimmed):
            if row[c] == '#':
                height += 1
        heights.append(height)

    return heights

        
for s in INPUT:
    schematic = s.splitlines()

    if all(map(lambda c: c == '#', schematic[0])) and all(map(lambda c: c == '.', schematic[-1])):
        locks.append(getheights(schematic))
    else:
        keys.append(getheights(schematic))

total = 0

for lock in locks:
    for key in keys:
        if all(map(lambda pair: pair[0] + pair[1] <= 5, zip(lock, key))):
            total += 1

print(total)

