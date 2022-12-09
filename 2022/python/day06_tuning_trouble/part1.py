import sys

line = input()

for i in range(0, len(line) - 4):
    if len(set(line[i : i + 4])) == 4:
        print(i + 4)
        break
