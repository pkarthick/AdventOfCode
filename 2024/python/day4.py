from data.day04 import PUZZLE_INPUT

from utils import Grid, HorizontalDirection, VerticalDirection

grid = Grid[str, str](PUZZLE_INPUT)

count1 = 0

is_xmas = lambda cell: cell in [['S', 'A', 'M', 'X'], ['X', 'M', 'A', 'S']]

for r in range(len(grid.cells)):
    for c in range(len(grid.cells[r])):
        count1 += 1 if is_xmas(grid.read_cells(r, c, HorizontalDirection.RIGHT, None, 4)) else 0
        count1 += 1 if is_xmas(grid.read_cells(r, c, None, VerticalDirection.DOWN, 4)) else 0
        count1 += 1 if is_xmas(grid.read_cells(r, c, HorizontalDirection.RIGHT, VerticalDirection.DOWN, 4)) else 0
        count1 += 1 if is_xmas(grid.read_cells(r, c, HorizontalDirection.LEFT, VerticalDirection.DOWN, 4)) else 0


print(count1)
count2 = 0

is_match = lambda cell: cell in [['S', 'A', 'M'], ['M', 'A', 'S']]

for r in range(len(grid.cells)):
    for c in range(len(grid.cells[r])):
        cells1 = grid.read_cells(r, c, HorizontalDirection.RIGHT, VerticalDirection.DOWN, 3)
        cells2 = grid.read_cells(r, c+2, HorizontalDirection.LEFT, VerticalDirection.DOWN, 3)
            
        if is_match(cells1) and is_match(cells2):
            count2 += 1

print(count2)
