import os
from functools import reduce

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 4
part = 1


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
lucky_numbers = list(map(int, lines[0].split(",")))
boards = [
    [
        list(map(lambda x: (int(x), False), filter(lambda x: x, l.split(" "))))
        for l in lines[s : s + 5]
    ]
    for s in range(2, len(lines), 6)
]


def markBoard(board, lucky_num):
    for row in board:
        for (ci, tup) in enumerate(row):
            if tup[0] == lucky_num:
                row[ci] = (tup[0], tup[1] or tup[0] == lucky_num)
                return (True, board)
    else:
        return (False, board)


def check_rows(board):
    for row in board:
        for (_, marked) in row:
            if not marked:
                break
        else:
            return True

    return False


def check_cols(board):
    for ci in range(0, 5):
        for row in board:
            if not row[ci][1]:
                break
        else:
            return True

    return False


def is_board_complete(board):
    return check_rows(board) or check_cols(board)


def get_unmarked(board):
    sum = 0
    for row in board:
        for (n, marked) in row:
            if not marked:
                sum += n

    return sum


def get_final_score(boards):
    for ln in lucky_numbers:
        for board in boards:
            (found, board) = markBoard(board, ln)
            if found and is_board_complete(board):
                return get_unmarked(board) * ln


actual = get_final_score(boards)

print(f"{kind} output:")
print(actual)

assert actual == expected

kind = "puzzle"

input = get_input(kind)
lines = input.splitlines()

lucky_numbers = list(map(int, lines[0].split(",")))
boards = [
    [
        list(map(lambda x: (int(x), False), filter(lambda x: x, l.split(" "))))
        for l in lines[s : s + 5]
    ]
    for s in range(2, len(lines), 6)
]

actual = get_final_score(boards)
print(f"{kind} output:")
print(actual)
