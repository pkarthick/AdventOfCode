import sys

trees = [[int(t) for t in line.strip()] for line in sys.stdin.readlines()]

rows = len(trees)
cols = len(trees[0])

visibility = [[True for _ in range(cols)] for _ in trees]


def checkVisibility(r, c):
    def fromTop():
        return all([trees[r1][c] < trees[r][c] for r1 in range(0, r)])

    def fromBottom():
        return all([trees[r1][c] < trees[r][c] for r1 in range(r + 1, rows)])

    def fromLeft():
        return all([trees[r][c1] < trees[r][c] for c1 in range(0, c)])

    def fromRight():
        return all([trees[r][c1] < trees[r][c] for c1 in range(c + 1, cols)])

    return fromTop() or fromBottom() or fromLeft() or fromRight()


for r, row in enumerate(trees[1:-1], start=1):

    for c, _ in enumerate(row[1:-1], start=1):

        visibility[r][c] = checkVisibility(r, c)

visibleTrees = sum([row.count(True) for row in visibility])

print(visibleTrees)
