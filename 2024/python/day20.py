from collections import Counter, defaultdict
from data.day20 import PUZZLE_INPUT, TEST_INPUT

import sys

sys.setrecursionlimit(10**6)

# INPUT = PUZZLE_INPUT
# MINSAVED = 100

# INPUT = TEST_INPUT
# MINSAVED = 1

INPUT = TEST_INPUT
MINSAVED = 50

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
                      

                elif cc == c:
                    wr, wc = (rr + 1, cc) if rr < r else (r + 1, c)
                    if G[wr][wc] == "#":
                        if pos[(r, c)] > pos[(rr, cc)]:
                            s = abs(pos[(rr, cc)] - pos[(r, c)]) - 2
                            if s >= MINSAVED:
                                pico_saved.update([s])
                        

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
            (rr != fr or cc != fc)
            and (rr, cc) not in visited
            and 0 <= rr < row_count
            and 0 <= cc < col_count
            and G[rr][cc] != "#"
        ):
            v = visited[:]
            v.append((rr, cc))
            fastest = traverse(r, c, rr, cc, v, fastest)

    return fastest

CHEATS = 20

fastest = traverse(sr, sc, sr, sc, [(sr, sc)], [])
pos = {k: v for k, v in zip(fastest, range(len(fastest)-1))}

cache = set()

saved_cache = dict()
cheat_mode_on = []
cheat_mode_used = []

sss = set()

def traverse_walls(fr, fc, r, c, visited_pair, chr, chc):

    # if not cheat_mode_on and cheat_mode_used:
    #     return

    visited, vws = visited_pair
    
    if len(vws) > CHEATS:
        return
    
    if ((fr,fc),(r, c)) in visited:
        return
    
    # if fwc >= 20:
    #     return fastest_pair

    saved_seconds = 84-len(visited)

    if r == er and c == ec:
        # if fwc == 0:
        # (visited, vwc)
        # print(vwc, len(visited))

        # only_seconds = "".join(map(lambda p: str(p[1]), visited))

        # if only_seconds not in cache:

            # print(only_seconds)

        # if saved_seconds >= 50:
        #     counter.update([saved_seconds])
            
            # print(counter)
        # else:
        #     pass

        return

    if saved_seconds < MINSAVED:
        return
        
    # if vwc >= 20:
    #     return fastest_pair

    v = visited.copy()
    v.add(((fr,fc),(r, c)))

    for rr, cc in [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]:
        if (
            (rr != fr or cc != fc)
            and 0 < rr < row_count-1
            and 0 < cc < col_count-1
            and ((r,c),(rr, cc)) not in visited
            # and (rr, cc) not in vws
        ):
            
            
            if cheat_mode_on:
                
                if len(vws) < CHEATS and G[rr][cc] == '#':
                    vws1 = vws[:]
                    vws1.append((rr,cc))
                    traverse_walls(fr, fc, rr, cc, (v, vws1), chr, chc)

                else:
                    # v = visited[:]
                    vws1 = vws[:]
                    vws1.append((rr,cc))
                   
                    if ((fr,fc),(rr, cc)) not in saved_cache:

                        if (rr,cc) in pos and (fr, fc) in pos and pos[(fr,fc)] < pos[(rr,cc)]:
                            
                            s = pos[(rr,cc)] - abs(rr-fr) - abs(cc-fc)

                            if s >= 50 and len(vws1) > 0 and G[fr][fc] != '#' and G[rr][cc] != '#':
                                
                                if ((fr,fc),(rr, cc)) in saved_cache:
                                    if s > saved_cache[((fr,fc),(rr, cc))]:
                                        saved_cache[((fr,fc),(rr, cc))] = s
                                else:
                                    saved_cache[((fr,fc),(rr, cc))] = s
                                
                    # traverse_walls(r, c, rr, cc, (v, vws1), chr, chc)

                    

            else:
                
                if not cheat_mode_used: 
                    if len(vws) < CHEATS and G[rr][cc] == '#' and r == chr and c == chc:
                        vws1 = vws[:]
                        vws1.append((rr,cc))
                        cheat_mode_on.append(True)
                        traverse_walls(r, c, rr, cc, (v, vws1), chr, chc)

                        cheat_mode_on.clear()
                        cheat_mode_used.append(True)
                
                        # cheat_mode_used = False
                        # cheat_mode_on = False
                        # v = visited[:]
                        # v.append(((r,c),(rr, cc)))
                        # traverse_walls(r, c, rr, cc, (v, vws), False, False)
                    else:
                        # v = visited[:]
                        # v.append(((r,c),(rr, cc)))
                        traverse_walls(r, c, rr, cc, (v, vws), chr, chc)
    

def saved_walls(fastest):
   
    # print(pos)

    pico_saved = Counter()

    for (i, (r, c)) in enumerate(fastest):

        traverse_walls(sr, sc, sr, sc, (set(), []), r, c)
        cheat_mode_on.clear()
        cheat_mode_used.clear()

        # for (j, (rr,cc)) in enumerate(fastest[i+1:]):
        #     (fv, fwc) = traverse_walls(r, c, r, c, rr, cc, ([], 0), ([], 0))
        #     if fwc <= 20:
        #         # if pos[(r, c)] > pos[(rr, cc)]:
        #             s = abs(abs(pos[(rr, cc)] - pos[(r, c)]) - 2)
        #             if s >= MINSAVED:
        #                 pico_saved.update([s])
           


    total = 0

    for k, v in pico_saved.items():
        print(v, k)
        total += v

    return total


fastest = traverse(sr, sc, sr, sc, [(sr, sc)], [])

# print(saved(fastest))

# print(saved_walls(fastest))

counter.clear()

# traverse_walls(sr, sc, sr, sc, ([], []), False, False)

# print(saved_cache)

saved_walls(fastest)

for k, v in saved_cache.items():
    print(k, v)

counter1 = defaultdict(list)

for k,v in saved_cache.items():
    # print((k,v))
    counter1[v].append([k])

for k, v in counter1.items():
    print(k, len(v))



