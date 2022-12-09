import sys

trees = [[int(t) for t in line.strip()] for line in sys.stdin.readlines()]

rows = len(trees)
cols = len(trees[0])


def scenicScore(r, c):

    top = 1

    h = trees[r - 1][c]

    if h < trees[r][c]:

        for r1 in range(r - 2, -1, -1):
            if trees[r1][c] < trees[r][c]:
                top += 1
            elif trees[r1][c] >= trees[r][c]:
                top += 1
                break

    bottom = 1

    h = trees[r + 1][c]

    if h < trees[r][c]:

        for r1 in range(r + 2, rows):
            if trees[r1][c] < trees[r][c]:
                bottom += 1
            elif trees[r1][c] >= trees[r][c]:
                bottom += 1
                break

    left = 1
    h = trees[r][c - 1]

    if h < trees[r][c]:

        for c1 in range(c - 2, -1, -1):
            if trees[r][c1] < trees[r][c]:
                left += 1
            elif trees[r][c1] >= trees[r][c]:
                left += 1
                break

    right = 1
    h = trees[r][c + 1]

    if h < trees[r][c]:
        for c1 in range(c + 2, cols):
            if trees[r][c1] < trees[r][c]:
                right += 1
            elif trees[r][c1] >= trees[r][c]:
                right += 1
                break

    return top * left * bottom * right


maxScenicScore = 0

for r, row in enumerate(trees[1:-1], start=1):

    for c, t in enumerate(row[1:-1], start=1):

        score = scenicScore(r, c)

        if score > maxScenicScore:
            # print(r, c, score)
            maxScenicScore = score


print(maxScenicScore)
