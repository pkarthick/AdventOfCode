import sys

groups = []
priorities = 0

lowerOffset = 97 - 1
upperOffset = 65 - 1 - 26


def getOffset(common):
    c = ord(list(common)[0])
    return c - (lowerOffset if c >= 97 else upperOffset)


def getPriority(l):
    half = len(l) // 2
    common = set(l[0:half]).intersection(set(l[half:]))
    return getOffset(common)


priorities = [getPriority(line.strip()) for line in sys.stdin.readlines()]

print(sum(priorities))
