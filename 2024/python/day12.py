from collections import defaultdict
from itertools import groupby
from data.day12 import PUZZLE_INPUT, TEST_INPUT

plant_locations = defaultdict(list)

lines = PUZZLE_INPUT.splitlines()

row_count = len(lines)
col_count = len(lines[0])

for r, line in enumerate(lines):
    for c, ch in enumerate(line):
        plant_locations[ch].append((r, c))
       
plants = {}

def neighbours8(r, c):
    return [(r1, c1) for (r1,c1) in [(r + 1, c + 1), (r + 1, c - 1), (r - 1, c + 1), (r - 1, c - 1), (r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)] if 0 <= r < row_count and 0 <= c < col_count]

def neighbours(r, c):
    return [(r1, c1) for (r1,c1) in [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)] if 0 <= r < row_count and 0 <= c < col_count]


def create_regions(plant, locs):

    plants = []
    plant_cells = set()
    others = []
    # locs.sort()


    while locs:

        (r, c) = locs[0]
        plant_cells.add((r, c))

        for rr, cc in neighbours(r, c):
            if (rr, cc) in locs:
                plant_cells.add((rr, cc))
                # locs.remove((rr, cc))

        for (r,c) in locs[1:]:

            # (r, c) = locs.pop()

            for rr, cc in neighbours(r, c):
                if (rr, cc) in plant_cells:
                    plant_cells.add((r, c))
                    break

            else:
                others.append((r, c))

        plants.append(plant_cells)

        locs = others
        others = []
        plant_cells = set([])

    return plants

        # modified = True

        # while modified:

    #     modified = False

    #     for r, c in others:
    #         for rr, cc in neighbours(r, c):
    #             if 0 <= rr < row_count and 0 <= cc < col_count and (rr, cc) in plant_cells:
    #                 others.remove((r, c))
    #                 plant_cells.add((r, c))
    #                 modified = True
    #                 break

    # if others:
    #     if len(others) == 1:
    #         return [plant_cells, sorted(others)]
    #     else:
    #         return [plant_cells] + create_regions(plant, others)
    # else:
    #     return [plant_cells]


def area_perimeter_1(plant, locs):
    area = 0
    perimeter = 0

    for (r, c) in locs:

        # print(ch, end='')
        area += 1

        for rr, cc in neighbours(r,c):

            if 0 <= rr < row_count and 0 <= cc < col_count:
                perimeter += 1 if lines[rr][cc] != plant else 0
            else:
                perimeter += 1

        # else:

        #     if ch != plant:
        #         print(' ', end='')
        #     else:
        #         print(' ', end='')

    # print()
    return area, perimeter


def area_perimeter_2(plant, locs):
    area = 0
    perimeter = 0

    all_lefts = []
    all_rights = []

    for r, line in enumerate(lines):

        lefts = []
        rights = []
        tops = []
        bottoms = []

        for c, ch in enumerate(line):

            if ch == plant and (r, c) in locs:
                lefts.append(c == 0 or line[c - 1] != plant)
                rights.append(c + 1 == len(line) or line[c + 1] != plant)
                tops.append(r == 0 or lines[r - 1][c] != plant)
                bottoms.append(r + 1 == row_count or lines[r + 1][c] != plant)
                # print(ch, end='')
                area += 1
            else:
                lefts.append(False)
                rights.append(False)
                tops.append(False)
                bottoms.append(False)
                # print(' ', end='')

        # gs = list(groupby(tops))
        perimeter += len([k for k, _ in groupby(tops) if k])
        perimeter += len([k for k, _ in groupby(bottoms) if k])

        all_lefts.append(lefts)
        all_rights.append(rights)

        # print()

    # print()

    for c in range(len(all_lefts[0])):

        ls = []
        rs = []

        for r in range(len(all_lefts)):
            ls.append(all_lefts[r][c])
            rs.append(all_rights[r][c])

        perimeter += len([k for k, _ in groupby(ls) if k])
        perimeter += len([k for k, _ in groupby(rs) if k])

    return (area, perimeter)


total1 = 0
total2 = 0

for plant, locs in plant_locations.items():

    regs = create_regions(plant, locs)

    for locs in regs:

        (area1, perimeter1) = area_perimeter_1(plant, locs)
        total1 += area1 * perimeter1

        (area2, perimeter2) = area_perimeter_2(plant, locs)
        total2 += area2 * perimeter2


print(total1)
print(total2)
