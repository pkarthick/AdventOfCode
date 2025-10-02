from data.day09 import PUZZLE_INPUT, TEST_INPUT
from dataclasses import dataclass

@dataclass
class Segment:
    num: int 
    size: int

@dataclass
class Block:
    segments: list[Segment]
    used: int
    free: int

def accomodate_block(start_block, end_block):
    if start_block.free >= end_block.used:
        start_block.segments.extend(end_block.segments)
        start_block.free -= end_block.used
        start_block.used += end_block.used
        end_block.segments.clear()
        end_block.free += end_block.used
        end_block.used = 0
        return True
    else:
        return False

class Data:

    def __init__(self, input):

        self.blocks: list[Block] = []
        self.data = []

        for ch in input:
            size = int(ch)
            
            if len(self.blocks) % 2 == 1:
                self.blocks.append(Block([], 0, size))
                self.data.extend([-1] * size)
            else:
                i = len(self.blocks) // 2
                self.blocks.append(Block([Segment(i, size)], size, 0))
                self.data.extend([i] * size)


    def checksum(self):

        start = 0
        end = len(self.data)-1
        total = 0

        while start <= end:

            while start <= end and self.data[start] != -1:
                total += self.data[start] * start
                start += 1

            while start <= end and self.data[start] == -1:
                
                while self.data[end] == -1:
                    end -= 1

                total += self.data[end] * start
                start += 1
                end -= 1

        return total

    def defrag(self):

        end = len(self.blocks) - 1
        start = 0

        while start < end:

            while end >= 0: 
                if self.blocks[end].used > 0:
                    break
                end -= 1
            else:
                break

            end_block = self.blocks[end]

            while start < end and self.blocks[start].free == 0:
                start += 1

            s = start
                                
            while s < end:

                start_block = self.blocks[s]

                if start_block.free >= end_block.used:
                    start_block.segments.extend(end_block.segments)
                    start_block.free -= end_block.used
                    start_block.used += end_block.used
                    end_block.segments.clear()
                    end_block.free += end_block.used
                    end_block.used = 0
                    end -= 1
                    break

                s += 1
                
            else:
                end -= 1
            

    def checksum2(self):

        self.defrag()

        i = 0
        total = 0

        for block in self.blocks:
            for segment in block.segments:
                for _ in range(segment.size):
                    total += i * segment.num 
                    i += 1
            
            i += block.free
            

        return total

data = Data(PUZZLE_INPUT)
print(data.checksum())
print(data.checksum2())