from data.day09 import PUZZLE_INPUT, TEST_INPUT
from dataclasses import dataclass

@dataclass
class Block:
    free: bool
    num: int 
    size: int
    start: int


class Data:

    def __init__(self, input):

        self.blocks: list[Block] = []
        self.data = []

        for ch in input:
            size = int(ch)
            
            if len(self.blocks) % 2 == 1:
                self.blocks.append(Block(True, -1, size, len(self.data)))
                self.data.extend([-1] * size)
            else:
                i = len(self.blocks) // 2
                self.blocks.append(Block(False, i, size, len(self.data)))
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

        while True:

            while end >= 0:
                if self.blocks[end].free:
                    end -= 1
                else:
                    break
            else:
                break

            blocks = self.blocks

            for start in range(end+1):

                start_block = blocks[start]
                end_block = blocks[end]

                if start_block.free:
                    if start_block.size == end_block.size:
                        start_block.num = end_block.num
                        end_block.num = -1
                        start_block.free = False
                        end_block.free = True
                        break
                    elif start_block.size > end_block.size:
                        st = start_block.start
                        start_size = start_block.size
                        start_block.num = end_block.num
                        end_block.num = -1
                        start_block.free = False
                        end_block.free = True
                        start_block.size = end_block.size
                        blocks.insert(start+1, Block(True, -1, start_size - end_block.size, st + end_block.size))
                        break
            else:
                end -= 1

    def checksum2(self):

        self.defrag()

        i = 0
        total = 0

        for block in self.blocks:
            if not block.free:
                for _ in range(block.size):
                    total += i * block.num 
                    i += 1
            else:
                i += block.size

        return total

data = Data(PUZZLE_INPUT)

print(data.checksum())

print(data.checksum2())



