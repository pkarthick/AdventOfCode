import sys


def diff(x1, x2):
    if x1 < 0:
        if x2 < 0:
            return abs(-x1 - -x2)
        else:
            return x2 + abs(x1)
    else:
        if x2 < 0:
            return x1 + abs(x2)
        else:
            return abs(x1 - x2)


limit = 4000000
# limit = 20


class Range:
    def __init__(self, low, high) -> None:
        self.low = low
        self.high = high

    def __repr__(self) -> str:
        return "(" + repr(self.low) + ", " + repr(self.high) + ")"


class Availability:
    def __init__(self, y) -> None:
        self.ranges = []
        self.y = y
        self.full = False

    def add(self, low, high):

        if self.full:
            return

        for rng in self.ranges:
            if low < rng.low and high >= rng.low and high <= rng.high:
                rng.low = low
                break
            elif low >= rng.low and low <= rng.high and high > rng.high:
                rng.high = high
                break
            elif low < rng.low and high > rng.high:
                rng.low = low
                rng.high = high
            elif low >= rng.low and high <= rng.high:
                return

        else:

            self.ranges.append(Range(low, high))

            self.ranges = list(sorted(self.ranges, key=lambda r: r.low))

        self.simplify()

        self.full = (
            len(self.ranges) == 1
            and self.ranges[0].low == 0
            and self.ranges[0].high == limit
        )
        # self.print()

    def simplify(self):

        while len(self.ranges) > 1:
            i = 0
            while i <= len(self.ranges) - 2:
                if self.ranges[i].high + 1 == self.ranges[i + 1].low:
                    self.ranges[i].high = self.ranges[i + 1].high
                    self.ranges.remove(self.ranges[i + 1])
                    break
                elif (
                    self.ranges[i].high >= self.ranges[i + 1].low
                    and self.ranges[i].high <= self.ranges[i + 1].high
                ):
                    self.ranges[i].high = self.ranges[i + 1].high
                    self.ranges.remove(self.ranges[i + 1])
                    break
                elif (
                    self.ranges[i].high >= self.ranges[i + 1].high
                    and self.ranges[i + 1].low <= self.ranges[i].high
                ):
                    self.ranges.remove(self.ranges[i + 1])
                    break

                i += 1
            else:
                break


spaces = [Availability(y) for y in range(limit + 1)]

unavailable = []

lines = sys.stdin.readlines()

for line in lines:
    line = line.strip()
    tokens = line.split()
    sensorx = int(tokens[2].rstrip(",").split("=")[1])
    sensory = int(tokens[3].rstrip(":").split("=")[1])

    beacony = int(tokens[-1].split("=")[1])
    beaconx = int(tokens[-2].rstrip(",").split("=")[1])

    strength = diff(sensorx, beaconx) + diff(sensory, beacony)

    unavailable.append((strength, sensorx, sensory))

unavailable.sort(reverse=True)

for (strength, sensorx, sensory) in unavailable:

    for s in range(strength + 1):

        low = sensorx - (strength - s)
        high = sensorx + (strength - s)

        if low < 0:
            low = 0

        if high > limit:
            high = limit

        if sensory - s >= 0:
            spaces[sensory - s].add(low, high)
        else:
            break

    for s in range(strength + 1):

        low = sensorx - (strength - s)
        high = sensorx + (strength - s)

        if low < 0:
            low = 0

        if high > limit:
            high = limit

        if sensory + s <= limit:
            spaces[sensory + s].add(low, high)
        else:
            break

for space in spaces:
    if not space.full:
        print((space.ranges[0].high + 1) * limit + space.y)
        break
