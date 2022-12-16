import sys

lines = []

minx = 1000
maxx = 0
miny = 1000
maxy = 0

d = {}

for l in sys.stdin.readlines():
    line = []
    for point in l.strip().split(" -> "):
        [x, y] = list(map(int, point.split(",")))

        d[(x, y)] = True

        if x < minx:
            minx = x
        elif x > maxx:
            maxx = x

        if y < miny:
            miny = y
        elif y > maxy:
            maxy = y

        line.append((x, y))

    for i in range(len(line) - 1):
        (x1, y1) = line[i]
        (x2, y2) = line[i + 1]

        if x1 == x2:
            (y1, y2) = (y1, y2) if y1 < y2 else (y2, y1)

            for y in range(y1, y2 + 1):
                d[(x1, y)] = True

        else:
            (x1, x2) = (x1, x2) if x1 < x2 else (x2, x1)

            for x in range(x1, x2 + 1):
                d[(x, y1)] = True

    lines.append(line)

for x in range(minx - 2000, maxx + 2000):
    d[(x, maxy + 2)] = True

srcx = 500
srcy = 0


def canMove(x, y):

    if (x, y + 1) not in d:
        y += 1
    elif (x - 1, y + 1) not in d:
        x -= 1
        y += 1
    # elif (x - 2, y + 1) not in d and (x - 1, y) not in d:
    #     x -= 2
    #     y += 1
    elif (x + 1, y + 1) not in d:
        x += 1
        y += 1
    # elif (x + 2, y + 1) not in d and (x + 1, y) not in d:
    #     x += 2
    #     y += 1
    else:
        return (False, x, y)

    return (True, x, y)


def countSand():

    counter = 0

    while True:

        x = srcx
        y = srcy

        while True:
            (can, x, y) = canMove(x, y)

            if can:

                if y == maxy + 2:
                    return counter

            else:

                if (x, y) == (srcx, srcy):
                    return counter + 1

                if (x, y) not in d:
                    d[(x, y)] = True
                    print(x, y)
                    counter += 1

                break

            # else:
            #     d[(x, y)] = True
            #     counter += 1


print()
print(countSand())
print()

# print(d)
# print()
