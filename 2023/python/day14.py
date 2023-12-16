from dataclasses import dataclass, field


def get_input(filename):
    f = open("../input/" + filename, "r")
    lines = f.read()
    f.close()
    return lines
    



def create_platform(input):
    return Platform([[ch for ch in l] for l in input.splitlines()])

@dataclass
class Platform:
    grid: list[list[str]]
    load_by_cycle: list[int] = field(default_factory=list)
    
    def get_rounded(self, row_range, col_range):
        rounded = []
        for r in row_range:
            for c in col_range:
                if self.grid[r][c] == 'O':
                    rounded.append((r,c))
        return rounded

    def roll_rock(self, rc, rr, cc):
        (r,c) = rc
        
        while r+rr >= 0 and c+cc >= 0 and r+rr < len(self.grid) and c+cc < len(self.grid[0]) and self.grid[r+rr][c+cc] == '.':
            self.grid[r+rr][c+cc] = 'O'
            self.grid[r][c] = '.'
            r += rr
            c += cc

    def cycle(self):

        for (rr, rc) in self.get_rounded(range(len(self.grid)), range(len(self.grid[0]))):
            self.roll_rock((rr,rc), -1, 0)

        for (rr, rc) in self.get_rounded(range(len(self.grid)), range(len(self.grid[0]))):
            self.roll_rock((rr,rc), 0, -1)

        for (rr, rc) in self.get_rounded(range(len(self.grid)-1, -1, -1), range(len(self.grid[0]))):
            self.roll_rock((rr,rc), 1, 0)

        for (rr, rc) in self.get_rounded(range(len(self.grid)), range(len(self.grid[0])-1, -1, -1)):
            self.roll_rock((rr,rc), 0, 1)

    def get_key(self):
        chars = [c for row in self.grid for c in row]
        return ''.join(chars)

    def loop(self, times):

        cache = dict()
        self.load_by_cycle.append(self.total_load())

        for r in range(1, times):
            self.cycle()
            key = self.get_key()
            self.load_by_cycle.append(self.total_load())
            value =  cache.get(key)
            if value != None:
                return (r - value, value, key)
                
            else:
                cache[key] = r
                
        return None

    def total_load(self):
        
        total = 0

        for (i, row) in enumerate(self.grid):
            count = len(list(filter(lambda ch: ch == 'O', row)))
            total += (len(self.grid) - i) * count
            
        return total
                
    def total_load_after_cycle_times(self, times):
        (lc, cycles_before_start, key) = self.loop(times)
        cycles_after_end = (times - cycles_before_start) % lc
        return self.load_by_cycle[cycles_before_start + cycles_after_end]

input = get_input("day14")

platform = create_platform(input)
for (rr, rc) in platform.get_rounded(range(len(platform.grid)), range(len(platform.grid[0]))):
    platform.roll_rock((rr,rc), -1, 0)

total_load = platform.total_load()
print("Part 1 Answer: ", total_load)

platform = create_platform(input)
total_load = platform.total_load_after_cycle_times(1000000000)
print("Part 2 Answer: ", total_load)

