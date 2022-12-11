import sys

cycle = 0

xargs = []

for row in sys.stdin.readlines():
    tokens = row.strip().split()
    if tokens[0] == "addx":
        cycle += 2
        xargs.append((cycle, int(tokens[1])))
    else:
        cycle += 1

display = [[" " for _ in range(40)] for _ in range(6)]

xreg = 1
index = 0
lineIndex = 0
cycle = 0

while index < len(xargs):

    if cycle >= xargs[index][0]:
        xreg += xargs[index][1]
        index += 1

    row = cycle // 40
    pixel = cycle % 40

    display[row][pixel] = "#" if pixel in [xreg - 1, xreg, xreg + 1] else " "

    cycle += 1


for row in display:
    print("".join(row))
