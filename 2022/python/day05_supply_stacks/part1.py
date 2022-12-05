import sys

lines = [line.removesuffix("\n") for line in sys.stdin.readlines()]

stackCount = (len(lines[0]) + 1) // 4

stacks = [[] for _ in range(0, stackCount)]

start = 0

for i, line in enumerate(lines):

    if line == "":
        start = i + 1
        break

    for j in range(0, stackCount):
        s = j * 4
        content = line[s : s + 4].replace(" ", "").removeprefix("[").removesuffix("]")

        if content != "":
            stacks[j].append(content)


def move(count, src, dest):
    available = len(stacks[src]) - 1
    count = count if count <= available else available
    stacks[dest] = stacks[src][:count][::-1] + stacks[dest]
    stacks[src] = stacks[src][count:]


for line in lines[start:]:

    frags = line.split(" ")
    [_, count, _, src, _, dest] = frags
    move(int(count), int(src) - 1, int(dest) - 1)

for stack in stacks:
    print(stack[0], end="")

print()
