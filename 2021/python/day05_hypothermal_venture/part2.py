import os
from functools import reduce

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 5
part = 2


def get_input(kind):

    file_path = f"{TEST_DATA_DIR}/{day}/{kind}_{part}.in"

    with open(file_path) as fo:
        return fo.read()


def get_output(kind):

    file_path = f"{TEST_DATA_DIR}/{day}/{kind}_{part}.out"

    with open(file_path) as fo:
        return fo.read()


kind = "sample"

input = get_input(kind)
expected = int(get_output(kind))

lines = input.splitlines()

def get_overlaps(lines):
    dict = {}

    for line in lines:
        [r1, r2] = line.split(" -> ")

        [x1, y1] = map(int, r1.split(','))
        [x2, y2] = map(int, r2.split(','))

        if y1 == y2:
            (x1, x2) = (x1, x2) if x1 <= x2 else (x2, x1)

        elif x1 == x2:
            (y1, y2) = (y1, y2) if y1 <= y2 else (y2, y1);

        # elif abs(x1-x2) == abs(y1-y2):

        xd = 0 if x1 == x2 else 1 if x1 < x2 else -1
        yd = 0 if y1 == y2 else 1 if y1 < y2 else -1

        x, y = x1, y1

        while True:

            dict[(x, y)] = dict[(x, y)] + 1 if (x, y) in dict else 1

            if x == x2 and y == y2: 
                break

            x += xd
            y += yd


    return len([v for v in dict.values() if v > 1])

actual = get_overlaps(lines)
print(f"{kind} output:")
print(actual)

assert actual == expected

kind = "puzzle"

input = get_input(kind)
lines = input.splitlines()
actual = get_overlaps(lines)
print(f"{kind} output:")
print(actual)
