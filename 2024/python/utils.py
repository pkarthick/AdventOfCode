from abc import ABC
from enum import Enum

class HorizontalDirection(Enum):
    LEFT = 0
    RIGHT = 1

class VerticalDirection(Enum):
    UP = 0
    DOWN = 1

class Grid[Row, Cell](ABC):
    
    def __init__(self, input: str) -> None:
        self.cells = [self.create_cells(row) for row in input.splitlines()]
        
    def read_cells(self, r: int, c: int, hd: HorizontalDirection, vd: VerticalDirection, count: int) -> list[Cell]:
        
        if r < 0 or r >= len(self.cells):
            return []
        
        if c < 0 or c >= len(self.cells[r]):
            return []
        
        match (vd, hd):

            case (None, None):
                return []
            
            case (None, HorizontalDirection.RIGHT):
                return [self.cells[r][c1] for c1 in range(c, c+count) if c1 >= 0 and c1 < len(self.cells[r])]
                
            case (None, HorizontalDirection.LEFT):
                return [self.cells[r][c1] for c1 in range(c-count, c+1) if c1 >= 0 and c1 < len(self.cells[r])]
                
            case (VerticalDirection.UP, None):
                return [self.cells [r1][c] for r1 in range(r-(count-1), r+1) if r1 >= 0 and r1 < len(self.cells)]
            
            case (VerticalDirection.DOWN, None):
                return [self.cells [r1][c] for r1 in range(r, r+count) if r1 >= 0 and r1 < len(self.cells)]
                
            case (VerticalDirection.UP, HorizontalDirection.RIGHT):
                return [self.cells [r-x][c+x] for x in range(count) if 0 <= r-x < len(self.cells) and 0 <= c+x < len(self.cells[r-x])]
            
            case (VerticalDirection.UP, HorizontalDirection.LEFT):
                return [self.cells [r-x][c-x] for x in range(count) if 0 <= r-x < len(self.cells) and 0 <= c-x < len(self.cells[r-x])]
            
            case (VerticalDirection.DOWN, HorizontalDirection.LEFT):
                return [self.cells [r+x][c-x] for x in range(count) if 0 <= r+x < len(self.cells) and 0 <= c-x < len(self.cells[r+x])]

            case (VerticalDirection.DOWN, HorizontalDirection.RIGHT):
                return [self.cells [r+x][c+x] for x in range(count) if 0 <= r+x < len(self.cells) and 0 <= c+x < len(self.cells[r+x])]

    def create_rows(self, s: str) -> list[Row]:
        return s.splitlines()
    
    def create_cells(self, s: Row) -> list[Cell]:
        return s

    def get_column(self, c: int) -> list[Cell]:
        return [self.cells[r][c] for r in range(len(self.cells))]