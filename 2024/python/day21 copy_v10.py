from collections import defaultdict
from dataclasses import dataclass
from functools import cache
from itertools import groupby, chain, pairwise
from data.day21 import TEST_INPUT, PUZZLE_INPUT

import sys
sys.setrecursionlimit(10 ** 6)

INPUT = TEST_INPUT

numpad_layout = [['7', '8', '9'], ['4','5','6'], ['1','2','3'], [' ', '0', 'A']]

numpad_locs = {}

numpad_locs['7'] = (0,0)
numpad_locs['8'] = (0,1)
numpad_locs['9'] = (0,2)

numpad_locs['4'] = (1,0)
numpad_locs['5'] = (1,1)
numpad_locs['6'] = (1,2)

numpad_locs['1'] = (2,0)
numpad_locs['2'] = (2,1)
numpad_locs['3'] = (2,2)

numpad_locs['0'] = (3,1)
numpad_locs['A'] = (3,2)

dirpad_locs = {}
dirpad_locs['^'] = (0,1)
dirpad_locs['A'] = (0,2)

dirpad_locs['<'] = (1,0)
dirpad_locs['v'] = (1,1)
dirpad_locs['>'] = (1,2)

dirmove = {}
dirmove[('^', 'A')] = 1
dirmove[('^', 'v')] = 1
dirmove[('^', '<')] = 2
dirmove[('^', '>')] = 2

dirmove[('v', 'A')] = 2
dirmove[('v', '^')] = 1
dirmove[('v', '<')] = 1
dirmove[('v', '>')] = 1

dirmove[('A', 'v')] = 2
dirmove[('A', '^')] = 1
dirmove[('A', '<')] = 3
dirmove[('A', '>')] = 1

dirmove[('>', 'v')] = 1
dirmove[('>', '^')] = 2
dirmove[('>', '<')] = 2
dirmove[('>', 'A')] = 1

dirmove[('<', 'v')] = 1
dirmove[('<', '^')] = 2
dirmove[('<', '>')] = 2
dirmove[('<', 'A')] = 3



@dataclass
class Strokes:
    level: int
    keys: str

    # def clone(self):
    #     return Strokes(self.level, self.keys)

# @cache    
def findpath(src: tuple[int, int], erc: tuple[int, int], left_skip:tuple[int, int], down_skip: tuple[int, int], path: str, paths: list[str]) -> list[str]:

    if src == erc:
        path.append('A')
        paths.append("".join(path))
        return paths
    
    else:

        (sr, sc) = src
        (er, ec) = erc

        if sr < er:
            if src != down_skip:
                p = path[:]
                p.append('v')
                paths = findpath((sr+1, sc), erc, left_skip, down_skip, p, paths)
            else:
                return paths
        elif sr > er:
            p = path[:]
            p.append('^')
            paths = findpath((sr-1, sc), erc, left_skip, down_skip, p, paths)
        

        if sc < ec:
            p = path[:]
            p.append('>')
            paths = findpath((sr, sc+1), erc, left_skip, down_skip, p, paths)
        elif sc > ec:
            if src != left_skip:
                p = path[:]
                p.append('<')
                paths = findpath((sr, sc-1), erc, left_skip, down_skip, p, paths)
            else:
                return paths
       

        return paths

@cache
def numpad_keystrokes(s: str, e:  str) -> list[str]:

    src = numpad_locs[s]
    erc = numpad_locs[e]

    strokes = findpath(src, erc, (3,1), (2,0), [], [])

    return strokes

    # return [Strokes(0, stroke) for stroke in strokes]

@cache
def dirpad_keystrokes(s: str, e:  str) -> str:

    if s == e:
        return 'AA'

    src = dirpad_locs[s]
    erc = dirpad_locs[e]

    strokes = findpath(src, erc, (0,1), (1,0), [], [])

    strokes.sort(key=len)

    return strokes[0]

@cache
def next_level_len(st):
    vs = dirpad_line(st)
    return min(vs,key=len)

@cache
def dirpad_keystrokes_by_level(s, e, level, base) -> list[str]:

    if ((s,e), level) in dir_cache:
        return dir_cache[((s,e), level)]

    else:
        pairs = dirpad_keystrokes_by_level(s, e, level-base, base)

        s = ''

        for (s1, e1) in pairwise(pairs):
            s += dir_cache[((s1,e1), base)]
            s += 'A'

        return s

@cache
def dirpad_keystrokes_25(s, e, level, base) -> int:


    tot = 0

    pairs1 = dir_cache[((s,e), base)]

    for (s1, e1) in pairwise('A' + pairs1):
        pairs2 = dir_cache[((s1,e1), base)]
        # tot += len(pairs2)
        for (s2, e2) in pairwise('A' + pairs2):
            pairs3 = dir_cache[((s2,e2), base)]
            # tot += len(pairs3)
            for (s3, e3) in pairwise('A' + pairs3):
                pairs4 = dir_cache[((s3,e3), base)]
                # tot += len(pairs4)
                for (s4, e4) in pairwise('A' + pairs4):
                    tot += len(dir_cache[((s4,e4), base)])
                    

    return tot



    # pairs = dir_cache[((s,e), level)]

    # tot = 0

    # for (s1, e1) in pairwise(pairs):
    #     tot += len(dir_cache[((s1,e1), 5)])
    #     tot += 1

    # return sum([len(dir_cache[((s1,e1), 5)])+1 ] for (s1, e1) in pairwise(pairs))
        


    # if ((s,e), level) in dir_cache:
    #     return dir_cache[((s,e), level)]

    # else:
    #     pairs = dirpad_keystrokes_25(s, e, level-base, base)

    #     # tot = 0

    #     # for (s1, e1) in pairwise(pairs):
    #     #     tot += len(dir_cache[((s1,e1), base)])
    #     #     tot += 1

    #     return sum([len(dir_cache[((s1,e1), base)])+1 ] for (s1, e1) in pairwise(pairs))



@cache
def dirpad_line(line) -> list[str]:

    all_strokes = ['']
    
    for (s, e) in pairwise(line):
        new_all_strokes = []
        for st in dirpad_keystrokes(s, e):
            for acc in all_strokes:
                new_all_strokes.append(acc + st)

        all_strokes = new_all_strokes

    return min(all_strokes, key=len)

@cache
def numpad_line(line) -> list[str]:

    all_strokes = numpad_keystrokes('A', line[0])
    
    for (s, e) in pairwise(line):
        new_all_strokes = []
        for st in numpad_keystrokes(s, e):
            for acc in all_strokes:
                new_all_strokes.append(acc + st)

        all_strokes = new_all_strokes

    return all_strokes




def flatten(seq: list[str|list[str]], res: list[str]) -> list[str]:

    for s in seq:
        if isinstance(s, list):
            res.extend(flatten(s, res))
        else:
            res.append(s)

    return res

def flatten_list(items):

    results = []

    for item in items:
        res = "".join(flatten(item, []))
        results.append(res)

    return results

def get_all_strokes(keys, strokesfunc) -> str:

    keys = ['A'] + keys
    all_strokes = [[]]
    new_all_strokes = []

    for (s, e) in pairwise(keys):
        new_all_strokes = []
        for strokes in strokesfunc(s, e):
            for s in all_strokes:
                s1 = s[:]
                s1.extend(strokes)
                new_all_strokes.append(s1)
        all_strokes = new_all_strokes

    return all_strokes


@cache
def get_all_dirpad_strokes(line: str) -> tuple[int, list]:

    all_strokes = dirpad_keystrokes('A', line[0])
    new_all_strokes = []

    for (s, e) in pairwise(line):
        new_all_strokes = []
        for (count1, strokes1) in dirpad_keystrokes(s, e):
            for (count, strokes) in all_strokes:
                if isinstance(strokes, list):
                    new_all_strokes.append((count + count1, strokes + [strokes1]))    
                else:
                    new_all_strokes.append((count + count1, [strokes, strokes1]))

        all_strokes = new_all_strokes

    return all_strokes



def display(xs):
    for s in xs:
        print(s, len(s))
    print()

def get_for_tuple(tup: tuple[int, list]):
    
    (count, robot_strokes) = tup

    if len(robot_strokes) == 1:
        robot_strokes = robot_strokes + ['']

    newcount = 0
    newstrokes = []

    for (robot_stroke, next_robot_stroke) in pairwise(robot_strokes):

        for (count1, robot_strokes1) in get_all_dirpad_strokes(robot_stroke + (next_robot_stroke[0] if next_robot_stroke else '') ):
            newcount += count1
            newstrokes.extend(robot_strokes1)
    
    return (newcount, newstrokes)

    # expanded_strokes[newcount].append((newcount, newstrokes))    

def get_robot_strokes(tups: list[tuple[int, list]], robots_count):

    if robots_count == 0:
        return tups
    
    # expanded_strokes = []

    # (mincount, _) = min(tups, key=lambda p: p[0])

    mincount = 10 ** 20
    min_tups = []
    
    for (count, strokes) in tups:
        if count == mincount:
            min_tups.append((count, strokes))
            # (newcount, newstrokes) = get_for_tuple((count, strokes))
            # if newcount < newmincount:
            #     min_tups.clear
            #     expanded_strokes.clear()
            # expanded_strokes.append(pair)
        elif count < mincount:
            mincount = count
            min_tups.clear()
            min_tups.append((count, strokes))
            



        # (newcount, newstrokes) = get_for_tuple(tup)

        # expanded_strokes[newcount].append((newcount, newstrokes))

    # minkey = min(expanded_strokes.keys())

    # shortest_expanded_strokes = expanded_strokes[minkey]

    new_tups = [get_for_tuple(tup) for tup in min_tups]

    return get_robot_strokes(new_tups, robots_count-1)


def get_next(pair):
    (s,e) = pair
    numpad_keystrokes()


dir_cache = defaultdict(list)

# dir_cache[(('A', 'v'), 1)] = '<vA'
# dir_cache[(('A', '^'), 1)] = '<A'
# dir_cache[(('A', '<'), 1)] = 'v<<A'
# dir_cache[(('A', '>'), 1)] = '^A'

# dir_cache[(('>', '>'), 1)] = 'A'
# dir_cache[(('<', '<'), 1)] = 'A'
# dir_cache[(('^', '^'), 1)] = 'A'
# dir_cache[(('v', 'v'), 1)] = 'A'
# dir_cache[(('A', 'A'), 1)] = 'A'

for i in range(1, 26):
    dir_cache[(('>', '>'), i)] = 'A' * i
    dir_cache[(('<', '<'), i)] = 'A' * i
    dir_cache[(('^', '^'), i)] = 'A' * i
    dir_cache[(('v', 'v'), i)] = 'A' * i
    dir_cache[(('A', 'A'), i)] = 'A' * i

for (s,e) in dirmove.keys():
    dir_cache[((s,e), 1)] = dirpad_keystrokes(s,e)

def next_level(level):

    for (s,e) in dirmove.keys():

        strokes = ''

        for (s1, e1) in pairwise('A' + dir_cache[((s,e), level-1)]):
            strokes += dir_cache[((s1,e1), 1)]

        dir_cache[((s,e), level)] = strokes

def next_level_5(level):

    for (s,e) in dirmove.keys():

        tot = 0

        for (s1, e1) in pairwise('A' + dir_cache[((s,e), level-5)]):
            tot += len(dir_cache[((s1,e1), 5)])

        return tot

def double_levels(level):

    for (s,e) in dirmove.keys():

        tot = 0

        for (s1, e1) in pairwise('A' + dir_cache[((s,e), level//2)]):
            tot += len(dir_cache[((s1,e1), level//2)])

        return tot

    
for l in range(2, 6):
    next_level(l)

# next_level_5(10)
# double_levels(20)
# next_level_5(25)


lines = INPUT.splitlines()

num_strokes_list = [numpad_line(line) for line in lines]
# print(num_strokes_list)

complexity = 0

for i, num_strokes in enumerate(num_strokes_list):

    mintot = 10 ** 20
    minst = ''

    for st in num_strokes:

        tot = 0
        
        for (s,e) in pairwise('A' + st):
            if s == e:
                tot += 1
            else:
                # tot += len(dir_cache[((s,e), 25)])
                # tot += len(dir_cache[((s,e), 2)]) 
                # tot += dirpad_keystrokes_25(s, e, 2, 1)
                tot += dirpad_keystrokes_25(s, e, 25, 5)
        
        if tot < mintot:
            print(tot, int(lines[i].strip('0')[:-1]))
            mintot = tot



    complexity += mintot * int(lines[i].strip('0')[:-1])

            
print(complexity)


# expander.process_numpad_strokes()
# expander.add_next_level()

# print(expander.levels[0])


# total = 0

# for i, line in enumerate(lines):

#     all_numpad_strokes = get_all_numpad_strokes(line)

#     robot_strokes = get_robot_strokes(all_numpad_strokes, 25)

#     for pair in robot_strokes:
#         x = pair
#         pass


#     short_len = min(map(lambda p: p[0], robot_strokes))

#     # all_first_robot_strokes = map(get_all_dirpad_strokes, all_numpad_strokes)
#     # all_second_robot_strokes = map(get_all_dirpad_strokes, chain.from_iterable(all_first_robot_strokes))
#     # short_len = min(map(len, chain.from_iterable(all_second_robot_strokes)))

#     print(short_len, int(lines[i].strip('0')[:-1]) )
#     total += short_len * int(lines[i].strip('0')[:-1])
    
# print(total)        
