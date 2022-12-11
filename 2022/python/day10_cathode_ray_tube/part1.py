import sys

xvalues = [0 for _ in range(10000)]
xvalues[1] = 1

cycle = 0

instructions = [(1, 1)]

for line in sys.stdin.readlines():
    tokens = line.strip().split()
    if tokens[0] == "addx":
        cycle += 2
        instructions.append((cycle, int(tokens[1])))
    else:
        cycle += 1

xreg = 0
signalStrength = 0

start = 0

for index, ins in enumerate(instructions):

    (cycle, xarg) = ins

    if cycle > 20:
        start = index
        signalStrength += xreg * 20
        break

    xreg += xarg

    if cycle == 20:
        signalStrength += xreg * 20
        start = index + 1
        break


signalStrengthThreshold = 60

for ins in instructions[start:]:

    (cycle, xarg) = ins

    if cycle < signalStrengthThreshold:
        xreg += xarg

    else:
        signalStrength += xreg * signalStrengthThreshold
        signalStrengthThreshold += 40
        xreg += xarg

print(signalStrength)
