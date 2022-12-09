import sys


class RopeBridge:
    def __init__(self) -> None:
        self.hr = 0
        self.tr = 0
        self.sr = 0
        self.hc = 0
        self.tc = 0
        self.sc = 0

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

        self.tailPos = {}
        self.markTail()

    def markTail(self):
        self.tailPos[(self.tr, self.tc)] = True

    def moveTail(self, rd, cd):

        if rd > 0:
            self.tr += 1
        elif rd < 0:
            self.tr -= 1

        if cd > 0:
            self.tc += 1
        elif cd < 0:
            self.tc -= 1

        self.markTail()

    def checkTail(self):

        rd, cd = self.hr - self.tr, self.hc - self.tc

        if (rd, cd) in self.closePositions:
            return
        else:
            self.moveTail(rd, cd)

    def moveInDir(self, dir, count):
        for _ in range(count):
            match dir:
                case "R":
                    self.hc += 1
                case "U":
                    self.hr += 1
                case "D":
                    self.hr -= 1
                case "L":
                    self.hc -= 1
            self.checkTail()


ropeBridge = RopeBridge()

for line in sys.stdin.readlines():
    [dir, count] = line.strip().split()
    ropeBridge.moveInDir(dir, int(count))

print(len(ropeBridge.tailPos))
