import sys

count = 0

for line in sys.stdin.readlines():
    l = line.strip().split(",")
    first = l[0].split("-")
    second = l[1].split("-")

    firstSet = set(x for x in range(int(first[0]), int(first[1]) + 1))
    secondSet = set(x for x in range(int(second[0]), int(second[1]) + 1))

    if (
        len(firstSet.difference(secondSet)) == 0
        or len(secondSet.difference(firstSet)) == 0
    ):
        count += 1

print(count)
