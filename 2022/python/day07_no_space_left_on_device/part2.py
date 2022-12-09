import sys

lines = [line.strip() for line in sys.stdin.readlines()]

dirs = []

subdirs = []
total = 0

sizeDict = {}

fileDict = {}

hierarchy = ["/"]

path = ""

for line in lines:
    tokens = line.split(" ")

    match tokens:

        case ["$", "cd", "/"]:
            hierarchy = ["/"]

        case ["$", "cd", ".."]:
            hierarchy = hierarchy[:-1]

        case ["$", "cd", dir]:
            hierarchy.append(hierarchy[-1] + dir + "/")

        case ["$", "ls"]:
            continue

        case ["dir", name]:
            continue

        case [size, filename]:
            filepath = hierarchy[-1] + filename

            if filepath not in fileDict:
                fileDict[filepath] = int(size)
                for h in hierarchy:
                    if h not in sizeDict:
                        sizeDict[h] = int(size)
                    else:
                        sizeDict[h] += int(size)

result = 0

available = 70000000
used = sizeDict["/"]

free = available - used
print(free)

sizeToFreeUp = 0

if free < 30000000:
    print("Size below threshold")
    sizeToFreeUp = 30000000 - free
    print("Free up: ", sizeToFreeUp)

sizeFreedUp = 0

for k in sizeDict:
    size = sizeDict[k]
    if size == sizeToFreeUp:
        sizeFreedUp = size
        break
    elif size >= sizeToFreeUp and (sizeFreedUp == 0 or size < sizeFreedUp):
        sizeFreedUp = size

print(sizeFreedUp)
