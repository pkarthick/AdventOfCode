from data.day14 import PUZZLE_INPUT

width = 101
height = 103

# width = 11
# height = 7

midx = width // 2
midy = height // 2

positions = []
velocities = []

for line in PUZZLE_INPUT.splitlines():
    p, v = list(map(lambda s: list(map(int, s[2:].split(","))), line.split(" ")))
    x, y = p
    xd, yd = v
    positions.append(p)
    velocities.append(v)


def christmas_tree_and_has_easter_egg(grid, continuous_count):

    count = 0

    for row in grid:
        for robots_count in row:
            if robots_count > 0:
                count += 1
                
            else:
                if count == continuous_count:
                    return True
                
                count = 0

    return False

def draw_christmas_tree(grid):
   
    print()
    
    for row in grid:
        for c in row:
            print(' '  if c == 0 else '*', end = '')

        print()

    print()

def part1(positions):


    q1 = 0
    q2 = 0
    q3 = 0
    q4 = 0


    for (x, y) in positions:
        if x < midx and y < midy:
            q1 += 1

        if x > midx and y < midy:
            q2 += 1

        if x > midx and y > midy:
            q4 += 1

        if x < midx and y > midy:
            q3 += 1

    print('Part1:', q1 * q2 * q3 * q4)

part1(positions)

seconds = 0


while seconds < 10000:


    for i in range(len(positions)):
        x, y = positions[i]
        
        xd, yd = velocities[i]
        
        x = (x + xd) % width
        y = (y + yd) % height

        positions[i] = (x, y)
        
    grid = [[0 for _ in range(width)] for _ in range(height)]

    for (c,r) in positions:
        grid[r][c] += 1

    seconds += 1

    if christmas_tree_and_has_easter_egg(grid, 9):
        print("Part2:", seconds)
        draw_christmas_tree(grid)
        
        break
  