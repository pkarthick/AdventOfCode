from collections import Counter
from data.day20 import PUZZLE_INPUT, TEST_INPUT

import sys

sys.setrecursionlimit(10**6)

INPUT = PUZZLE_INPUT
MINSAVED = 100

# INPUT = TEST_INPUT
# MINSAVED = 1

G = []

sr = 0
sc = 0
er = 0
ec = 0

for r, line in enumerate(INPUT.splitlines()):
    row = []
    for c, ch in enumerate(line):
        if ch == "S":
            sr = r
            sc = c

        elif ch == "E":
            er = r
            ec = c

        row.append(ch)

    G.append(row)

row_count = len(G)
col_count = len(G[0])


def saved(fastest):

    pos = {k: v for k, v in zip(fastest, range(1, len(fastest) + 1))}

    # print(pos)

    pico_saved = Counter()

    for r, c in fastest:
        for rr, cc in [(r + 2, c), (r - 2, c), (r, c + 2), (r, c - 2)]:
            if (rr, cc) in fastest:
                if rr == r:
                    wr, wc = (r, cc + 1) if cc < c else (r, c + 1)
                    if G[wr][wc] == "#":
                        if pos[(r, c)] > pos[(rr, cc)]:
                            s = abs(pos[(rr, cc)] - pos[(r, c)]) - 2
                            if s >= MINSAVED:
                                pico_saved.update([s])
                        # elif rr==er and cc == ec:
                        #     s = abs(pos[(rr, cc)] - pos[(r, c)]) - 2
                        #     if s >= MINSAVED:
                        #         pico_saved.update([s])

                elif cc == c:
                    wr, wc = (rr + 1, cc) if rr < r else (r + 1, c)
                    if G[wr][wc] == "#":
                        if pos[(r, c)] > pos[(rr, cc)]:
                            s = abs(pos[(rr, cc)] - pos[(r, c)]) - 2
                            if s >= MINSAVED:
                                pico_saved.update([s])
                        # elif rr==er and cc == ec:
                        #     s = abs(pos[(rr, cc)] - pos[(r, c)]) - 2
                        #     if s >= MINSAVED:
                        #         pico_saved.update([s])

    total = 0

    for k, v in pico_saved.items():
        print(v, k)
        total += v

    return total


counter = Counter()

# fastest = row_count * col_count


def traverse(fr, fc, r, c, visited, fastest):

    if r == er and c == ec:
        if len(fastest) == 0 or len(visited) < len(fastest):
            fastest = visited
            counter.update([len(visited)])

        return fastest

    for rr, cc in [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]:
        if (
            (rr != fr or cc != c)
            and (rr, cc) not in visited
            and 0 <= rr < row_count
            and 0 <= cc < col_count
            and G[rr][cc] != "#"
        ):
            v = visited[:]
            v.append((rr, cc))
            fastest = traverse(r, c, rr, cc, v, fastest)

    return fastest


fastest = traverse(sr, sc, sr, sc, [(sr, sc)], [])

print(saved(fastest))
