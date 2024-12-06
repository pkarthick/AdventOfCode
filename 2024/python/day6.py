from collections import Counter
from enum import Enum
from data.day06 import PUZZLE_INPUT
from utils import Grid

import sys

sys.setrecursionlimit(10**6)

INPUT = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

class Direction(Enum):
    NORTH = 0
    EAST = 1
    SOUTH = 2
    WEST = 3

lines = PUZZLE_INPUT.splitlines()

rowcount = len(lines)
colcount = len(lines[0])

obstructions = [[ch == '#' for ch in line] for line in lines]

for r in range(rowcount):
    for c in range(colcount):
        if lines[r][c] == '^':
            sr = r
            sc = c
            
def track_visited(r, c, direction, visited):


    visited.append((r,c,direction))

    match direction:
        case Direction.NORTH:

            if r == 0: return False

            for rr in range(r, -1, -1):
                if rr > 0 and obstructions[rr-1][c]: 
                    if not obstructions[rr][c]:
                        return track_visited(rr, c, Direction.EAST, visited)
                else:
                    visited.append((rr,c,direction))
        


        case Direction.EAST:

            if c == len(obstructions[r]) : return False

            for cc in range(c, len(obstructions[r])):

                if cc < len(obstructions[r])-1 and obstructions[r][cc+1]:
                    if not obstructions[r][cc]:
                        return track_visited(r, cc, Direction.SOUTH, visited)  
                else:
                    visited.append((r,cc,direction))
            
                    

        case Direction.SOUTH:

            if r == len(obstructions) : return False
            
            for rr in range(r, len(obstructions)):
                if rr < len(obstructions)-1 and obstructions[rr+1][c]:
                    if not obstructions[rr][c]:
                        return track_visited(rr, c, Direction.WEST, visited)          
                else:
                    visited.append((rr,c,direction))
            
                    
    
        case Direction.WEST:

            if c == 0: return False

            for cc in range(c, -1, -1):
                if cc > 0 and obstructions[r][cc-1]:
                    if not obstructions[r][cc]:
                        return track_visited(r, cc, Direction.NORTH, visited)
                else:
                    visited.append((r,cc,direction))

def track_turns(r, c, direction, turns):

    if (r,c,direction) in turns:
        return True

    turns.add((r,c,direction))


    match direction:
        case Direction.NORTH:

            for rr in range(r, -1, -1):
                if rr > 0 and obstructions[rr-1][c] and not obstructions[rr][c]:
                        return track_turns(rr, c, Direction.EAST, turns)
               

        case Direction.EAST:

            for cc in range(c, colcount):

                if cc < colcount-1 and obstructions[r][cc+1] and not obstructions[r][cc]:
                        return track_turns(r, cc, Direction.SOUTH, turns)  
               
                     

        case Direction.SOUTH:
            
            for rr in range(r, rowcount):
                if rr < rowcount-1 and obstructions[rr+1][c] and not obstructions[rr][c]:
                        return track_turns(rr, c, Direction.WEST, turns)
               
                    
    
        case Direction.WEST:

            for cc in range(c, -1, -1):
                if cc > 0 and obstructions[r][cc-1] and not obstructions[r][cc]:
                        return track_turns(r, cc, Direction.NORTH, turns)
               
             
    return False                 



visited = list()
turns = set()

track_visited(sr, sc, Direction.NORTH, visited)

uniq = set(map(lambda trip: (trip[0], trip[1]), visited))

print(len(uniq))

loops = set()
loop_count = 0

for (r,c) in uniq:
        
    if lines[r][c] != '#':

        obstructions[r][c] = True

        if track_turns(sr, sc, Direction.NORTH, set()):
            loop_count += 1
        
        
        obstructions[r][c] = False

print(loop_count)
