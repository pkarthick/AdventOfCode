from data.day02 import PUZZLE_INPUT
from utils import Grid
from itertools import pairwise

def is_safe(levels):

    if levels[0] < levels[1]:
        return all([-3 <= a-b <= -1 for (a,b) in pairwise(levels)])
    else:
        return all([1 <= a-b <= 3 for (a,b) in pairwise(levels)])

count1 = 0
count2 = 0

class MyGrid(Grid):
    def create_cells(self, s: str) -> list[int]:
        return list(map(int, s.split()))

grid = MyGrid(PUZZLE_INPUT)

for levels in grid.cells:
    
    if is_safe(levels):
        count1 += 1
        count2 += 1
    else:
        for i in range(len(levels)):
            if is_safe(levels[0:i] + levels[i+1:]):
                count2 += 1
                break

print(count1)            
print(count2)