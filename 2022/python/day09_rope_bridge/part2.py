import sys


class Knot:
    def __init__(self) -> None:
        self.r = 0
        self.c = 0

        self.closePositions = [
            (0, 0),
            (0, 1),
            (1, 0),
            (0, -1),
            (-1, 0),
            (1, 1),
            (1, -1),
            (-1, 1),
            (-1, -1),
        ]

    def makeAdjustment(self, rd, cd):
        if rd > 0:
            self.r += 1
        elif rd < 0:
            self.r -= 1

        if cd > 0:
            self.c += 1
        elif cd < 0:
            self.c -= 1

    def move(self, headPos):

        if headPos != None:
            (hr, hc) = headPos
            (rd, cd) = hr - self.r, hc - self.c

            if (rd, cd) in self.closePositions:
                return False
            else:
                self.makeAdjustment(rd, cd)
                return True
        else:
            self.makeAdjustment(rd, cd)
            return True

    def moveInDir(self, dir, headPos):

        if headPos == None:
            match dir:
                case "R":
                    self.c += 1
                case "L":
                    self.c -= 1
                case "U":
                    self.r -= 1
                case "D":
                    self.r += 1
            return True
        return self.move(headPos)


class RopeBridge:
    def markTail(self, knot):
        self.tailPos[(knot.r, knot.c)] = True

    def __init__(self) -> None:
        self.tailPos = {(0, 0): True}
        self.knots = [Knot() for _ in range(10)]

    def moveInDir(self, dir, count):
        for _ in range(count):
            headPos = None
            for knot in self.knots:
                if not knot.moveInDir(dir, headPos):
                    break
                headPos = (knot.r, knot.c)

            self.markTail(self.knots[-1])


bridge = RopeBridge()

for line in sys.stdin.readlines():
    [dir, count] = line.strip().split()
    bridge.moveInDir(dir, int(count))

print(len(bridge.tailPos))
