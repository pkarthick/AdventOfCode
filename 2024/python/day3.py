from data.day03 import PUZZLE_INPUT

import re

def calc(input:str) -> int:
    total = 0

    for x in re.findall(r'mul\((\d+),(\d+)\)', input):
        n1, n2 = map(int, x)
        total += n1 * n2

    return total

print(calc(PUZZLE_INPUT))

result = 0

for s in PUZZLE_INPUT.split('do()'):
    index = s.find("don't()")
    ss = s if index==-1 else s[:index]
    result += calc(ss)

print(result)