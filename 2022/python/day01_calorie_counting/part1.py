import sys

allLines = (line.strip() for line in sys.stdin.readlines())

maxCalorie = 0
calorie = 0

for line in allLines:
    if line == "":
        if calorie > maxCalorie:
            maxCalorie = calorie
        calorie = 0
        continue
    calorie += int(line)

if calorie > maxCalorie:
    maxCalorie = calorie

print(maxCalorie)
