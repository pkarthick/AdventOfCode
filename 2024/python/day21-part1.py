from functools import cache
from itertools import groupby, chain, pairwise
from data.day21 import TEST_INPUT, PUZZLE_INPUT

INPUT = PUZZLE_INPUT

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

def findpath(src: tuple[int, int], erc: tuple[int, int], left_skip:tuple[int, int], down_skip: tuple[int, int], path: str, paths: list[str]):

    if src == erc:
        path.append('A')
        paths.append("".join(path))
        return paths

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
def numpad_keystrokes(s: str, e:  str):

    src = numpad_locs[s]
    erc = numpad_locs[e]

    return findpath(src, erc, (3,1), (2,0), [], [])

@cache
def dirpad_keystrokes(s: str, e:  str):

    src = dirpad_locs[s]
    erc = dirpad_locs[e]

    return findpath(src, erc, (0,1), (1,0), [], [])

def get_all_strokes(line, strokesfunc) -> str:

    all_strokes = [""]
    new_all_strokes = []

    for (s, e) in pairwise("A" + line):
        new_all_strokes = []
        for strokes in strokesfunc(s, e):
            for s in all_strokes:
                new_all_strokes.append(s + strokes)
        all_strokes = new_all_strokes

    # groups = groupby(sorted(all_strokes), key=len)
    # group = next(groups)

    # xs = list(group[1])
    # display(xs)

    return all_strokes

def get_all_numpad_strokes(line) -> list[str]:
    return get_all_strokes(line, numpad_keystrokes)

def get_all_dirpad_strokes(line) -> list[str]:
    return get_all_strokes(line, dirpad_keystrokes)


def display(xs):
    for s in xs:
        print(s, len(s))
    print()

# print(list(map(lambda s: "A" + s, INPUT.splitlines())))

lines = INPUT.splitlines()
# lines[0] = "A" + lines[0]

# all_numpad_strokes = get_all_strokes(lines, numpad_keystrokes)
# # all_numpad_strokes = get_all_strokes(map(lambda s: "A" + s, INPUT.splitlines()), numpad_keystrokes)
# all_dirpad_strokes = get_all_strokes(all_numpad_strokes, dirpad_keystrokes)

# display(all_dirpad_strokes)

total = 0

for i, line in enumerate(lines):

    # line = "A" + line

    all_numpad_strokes = get_all_numpad_strokes(line)

    all_first_robot_strokes = map(get_all_dirpad_strokes, all_numpad_strokes)
    all_second_robot_strokes = map(get_all_dirpad_strokes, chain.from_iterable(all_first_robot_strokes))

    # xs = list(all_second_robot_strokes)
    short_len = min(map(len, chain.from_iterable(all_second_robot_strokes)))

    print(short_len, int(lines[i].strip('0')[:-1]) )
    total += short_len * int(lines[i].strip('0')[:-1])
    
print(total)        
