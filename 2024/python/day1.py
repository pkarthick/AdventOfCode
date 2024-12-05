from data.day01 import PUZZLE_INPUT

from collections import Counter
from utils import Grid

class MyGrid(Grid):
    def create_cells(self, s: str) -> list[int]:
        return list(map(int, s.split()))

def part1():

    grid = MyGrid(PUZZLE_INPUT)
    
    nums1 = grid.get_column(0)
    nums2 = grid.get_column(1)

    nums1.sort()
    nums2.sort()

    total = sum([abs(num1-num2) for num1, num2 in zip(nums1, nums2)])
            
    print(total)
    

def part2():

    grid = MyGrid(PUZZLE_INPUT)

    nums1 = grid.get_column(0)
    counter = Counter(grid.get_column(1))
    total = sum([num * counter[num] for num in nums1 if num in counter])
        
    print(total)
    
def part1_and_2():

    grid = MyGrid(PUZZLE_INPUT)
    
    nums1 = grid.get_column(0)
    nums2 = grid.get_column(1)

    counter = Counter(grid.get_column(1))
    nums1.sort()
    nums2.sort()

    total1 = 0
    total2 = 0

    for num1, num2 in zip(nums1, nums2):
        total1 += abs(num1-num2)
        total2 += num1 * counter.get(num1, 0) 
        
    print(total1)
    print(total2)


# part1()
# part2()
part1_and_2()