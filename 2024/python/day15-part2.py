from data.day15 import PUZZLE_INPUT, TEST_INPUT
import sys

sr = 0
sc = 0

sys.setrecursionlimit(10**6)

top, bottom = PUZZLE_INPUT.split("\n\n")

G = []

for r, line in enumerate(top.splitlines()):
    row = []
    for c, ch in enumerate(line):
        match ch:
            case "@":
                sr = r
                sc = c * 2
                row.append(".")
                row.append(".")

            case "O":
                row.append("[")
                row.append("]")
            case _:
                row.append(ch)
                row.append(ch)

    G.append(row)

G[sr][sc] = "@"

r, c = sr, sc


def move_up(r1, c1, moves):

    if (r1, c1) not in moves:
        moves.append((r1, c1))

    if r1 == 1:
        return False

    # match G[r1][c1]:

    # case "[":
    if G[r1 - 1][c1] == "#":
        return False

    elif G[r1 - 1][c1] == "[":
        return move_up(r1 - 1, c1, moves)

    elif G[r1 - 1][c1] == "]":
        if G[r1 - 1][c1 + 1] == "[":
            res1 = move_up(r1 - 1, c1 - 1, moves)
            res2 = move_up(r1 - 1, c1 + 1, moves)

            return res1 and res2

        elif G[r1 - 1][c1 + 1] == "#":
            return False

        else:
            return move_up(r1 - 1, c1 - 1, moves)

    else:
        if G[r1 - 1][c1] == "." and G[r1 - 1][c1 + 1] == ".":
            return True
        elif G[r1 - 1][c1] == "." and G[r1 - 1][c1 + 1] == "#":
            return False
        elif G[r1 - 1][c1] == "." and G[r1 - 1][c1 + 1] == "[":
            return move_up(r1 - 1, c1 + 1, moves)
        else:
            return move_up(r1 - 1, c1, moves)


def move_down(r1, c1, moves):

    if (r1, c1) not in moves:
        moves.append((r1, c1))

    if r1 == len(G) - 1:
        return False

    if G[r1 + 1][c1] == "#":
        return False

    elif G[r1 + 1][c1] == "[":
        return move_down(r1 + 1, c1, moves)

    elif G[r1 + 1][c1] == "]":
        if G[r1 + 1][c1 + 1] == "[":
            res1 = move_down(r1 + 1, c1 - 1, moves)
            res2 = move_down(r1 + 1, c1 + 1, moves)
            return res1 and res2

        elif G[r1 + 1][c1 + 1] == "#":
            return False

        else:
            return move_down(r1 + 1, c1 - 1, moves)

    else:
        if G[r1 + 1][c1] == "." and G[r1 + 1][c1 + 1] == ".":
            return True
        elif G[r1 + 1][c1] == "." and G[r1 + 1][c1 + 1] == "#":
            return False
        elif G[r1 + 1][c1] == "." and G[r1 + 1][c1 + 1] == "[":
            return move_down(r1 + 1, c1 + 1, moves)
        else:
            return move_down(r1 + 1, c1, moves)


def move_left(r, c):

    if c == 1:
        return (False, [(r, c)])

    match G[r][c]:
        case "]" | "[":
            return move_left(r, c - 1)
        case "#":
            return (False, [(r, c)])
        case ".":
            return (True, [(r, c)])


def move_right(r, c):

    if c == len(G[0]) - 1:
        return (False, r, c)

    match G[r][c]:
        case "]" | "[":
            return move_right(r, c + 1)
        case "#":
            return (False, [(r, c)])
        case ".":
            return (True, [(r, c)])


def check():
    for r, row in enumerate(G):
        for c, ch in enumerate(row):
            match ch:
                case "[" if G[r][c + 1] != "]":
                    return False
                case "]" if G[r][c - 1] != "[":
                    return False

    return True


# l = len(bottom)


for i, ch in enumerate(bottom):

    if ch == "\n":
        continue

    # if not check():
    #     pass

    # print(i, l)

    match ch:
        case "<":
            (res, [(r1, c1)]) = move_left(r, c - 1)
            if res:
                while c1 < c:
                    G[r1][c1] = G[r1][c1 + 1]
                    c1 += 1

                G[r][c] = "."
                c -= 1

        case ">":
            (res, [(r1, c1)]) = move_right(r, c + 1)
            if res:
                while c1 > c:
                    G[r1][c1] = G[r1][c1 - 1]
                    c1 -= 1

                G[r][c] = "."
                c += 1

        case "^":

            if G[r - 1][c] == "#":
                continue

            if G[r - 1][c] == ".":
                G[r][c] = "."
                r -= 1
                G[r][c] = "@"
                continue

            moves = []

            cx = c

            if G[r - 1][c] == "]":
                cx -= 1

            if move_up(r - 1, cx, moves):

                moves.sort(reverse=True)

                while moves:
                    (r1, c1) = moves.pop()

                    G[r1 - 1][c1] = G[r1][c1]
                    G[r1 - 1][c1 + 1] = G[r1][c1 + 1]
                    G[r1][c1] = "."
                    G[r1][c1 + 1] = "."

                G[r - 1][c] = "@"
                G[r][c] = "."
                r -= 1

        case "v":

            if G[r + 1][c] == "#":
                continue

            if G[r + 1][c] == ".":
                G[r][c] = "."
                r += 1
                G[r][c] = "@"
                continue

            moves = []

            cx = c

            if G[r + 1][c] == "]":
                cx -= 1

            if move_down(r + 1, cx, moves):

                moves.sort()

                while moves:
                    (r1, c1) = moves.pop()

                    G[r1 + 1][c1] = G[r1][c1]
                    G[r1 + 1][c1 + 1] = G[r1][c1 + 1]
                    G[r1][c1] = "."
                    G[r1][c1 + 1] = "."

                G[r + 1][c] = "@"
                G[r][c] = "."
                r += 1

total = 0

for r, row in enumerate(G):
    for c, ch in enumerate(row):
        if ch == "[":
            total += r * 100 + c

print(total)
