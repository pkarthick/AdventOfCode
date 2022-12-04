import sys

lowerOffset = 97 - 1
upperOffset = 65 - 1 - 26


def getOffset(common):
    c = ord(list(common)[0])
    return c - (lowerOffset if c >= 97 else upperOffset)


groups = []
priorities = 0

for line in sys.stdin.readlines():
    setNew = set(line.strip())
    for group in groups:
        groupLen = len(group)
        if groupLen == 3:
            continue
        else:
            common = group[0].intersection(setNew)
            if len(common) > 0:
                if groupLen == 1:
                    group.append(setNew)
                    break
                elif groupLen == 2:
                    common = common.intersection(group[1])
                    if len(common) == 1:
                        group.append(setNew)
                        priorities += getOffset(common)
                        break

    else:
        groups.append([set(line)])

print(priorities)
