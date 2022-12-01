import sys

allLines = (line.strip() for line in sys.stdin.readlines())

calories = []
calorie = 0

for line in allLines:
    if line == "":
        calories.append(calorie)
        calorie = 0
        continue
    calorie += int(line)

calories.append(calorie)
calories.sort(reverse=True)

print(sum(calories[:3]))
