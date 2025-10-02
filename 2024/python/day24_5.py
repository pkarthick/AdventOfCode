from dataclasses import dataclass
from functools import reduce
from itertools import dropwhile
from operator import add
from typing import Callable

from data.day24 import PUZZLE_INPUT


def get_decimal(binaries: list[int]):
    return reduce(lambda acc, d: acc * 2 + d, binaries)

def gate_output(gate: str, wire1: int, wire2: int) -> int:
    match gate:
        case "AND":
            return wire1 & wire2
        case "OR":
            return wire1 | wire2
        case "XOR":
            return wire1 ^ wire2
        case _:
            raise Exception(f"Unexpected gate: {gate}")

def is_xy(w):
    return w[0] in ["x", "y"]

@dataclass
class Device:
    in_wires: dict[str, int]
    in_wire_setup: dict[str, tuple[str, str, str]]
    constants: set[str]
    xs: list[int]
    ys: list[int]
    zs: list[int]
    expected: list[int]
    operation: Callable[[int, int], int]
    usage: dict[int, set[str]]

    def __init__(
        self,
        wires: dict[str, int],
        wire_setup: dict[str, tuple[str, str, str]],
        operation: Callable[[int, int], int],
    ):
        self.wires = wires
        self.wire_setup = wire_setup
        self.operation = operation
        self.usage = {}
        self.swapped = set()

        constants: set[str] = set()

        for wire, ws in wire_setup.items():
            (i1, _, i2) = ws
            if i1[0] in ["x", "y"] and i2[0] in ["x", "y"]:
                constants.add(wire)

        self.constants = constants
        self.xs = [wires[f"x{x:02d}"] for x in range(44, -1, -1)]
        self.ys = [wires[f"y{y:02d}"] for y in range(44, -1, -1)]
        self.zs = []

        x_decimal = get_decimal(self.xs)
        y_decimal = get_decimal(self.ys)
        self.expected_decimal = operation(x_decimal, y_decimal)

        expected = str(bin(self.expected_decimal))[2:]
        self.expected = [ord(c) - 48 for c in expected]

    def clone(self):
        wire_setup1 = {k: (v[0], v[1], v[2]) for k, v in self.wire_setup.items()}
        wires1 = {k: v for k, v in self.wires.items()}
        swapped = self.swapped
        device = Device(wires1, wire_setup1, self.operation)
        device.swapped = set([x for x in self.swapped])
        return device

    def is_recursive(self, g1: str, g2: str, visited: set[str]) -> bool:

        visited.add(g1)
        visited.add(g2)
        #
        # if g1 in visited or g2 in visited:
        #     return True


        (i11, _, i12) = self.wire_setup[g1]

        if i11 in visited or i12 in visited:
            return True
        else:
            if i11[0] not in ["x", "y"] and i12[0] not in ["x", "y"]:
                if self.is_recursive(i11, g1, visited) or self.is_recursive(i12, g1, visited):
                    return True

        (i1, _, i2) = self.wire_setup[g2]

        if i1 in visited or i2 in visited:
            return True
        else:
            if i1[0] not in ["x", "y"] and i2[0] not in ["x", "y"]:
                return self.is_recursive(i1, g2, visited) or self.is_recursive(i2, g2, visited)
            else:
                return False


    def getvalue(self, w: str):

        # print(w)

        # if w in self.wires:
        #     return self.wires[w]

        (i1, op, i2) = self.wire_setup[w]

        match (i1 in self.wires, i2 in self.wires):
            case (True, True):
                self.wires[w] = gate_output(
                    op, self.wires[i1], self.wires[i2]
                )

            case (True, False):
                self.wires[w] = gate_output(
                    op, self.wires[i1], self.getvalue(i2)
                )

            case (False, True):
                self.wires[w] = gate_output(
                    op, self.getvalue(i1), self.wires[i2]
                )

            case (False, False):
                self.wires[w] = gate_output(
                    op, self.getvalue(i1), self.getvalue(i2)
                )

        return self.wires[w]

    def show(self, w: str):
        for wire, wire_setup in self.wire_setup.items():
            (xg, op, yg) = wire_setup

            if xg == w:
                print(xg, f"{op:3s}", yg, "->", wire)
            if yg == w:
                print(xg, f"{op:3s}", yg, "->", wire)
            if wire == w:
                print(xg, f"{op:3s}", yg, "->", wire)

    def swap(self, w1: str, w2: str):

        cloned = self.clone()

        (x1, xop, x2) = self.wire_setup[w1]
        (y1, yop, y2) = self.wire_setup[w2]

        cloned.wire_setup[w1] = (y1, yop, y2)
        cloned.wire_setup[w2] = (x1, xop, x2)

        cloned.swapped.add(w1)
        cloned.swapped.add(w2)

        return cloned

    def get_binary(self):
        return reduce(lambda acc, z: acc * 2 + z, self.zs, 0)

    def run(self):

        self.wires = {k:v for k, v in self.wires.items() if is_xy(k) }

        zs = [f"z{z:02d}" for z in range(46)]

        for z in range(46):
            wire = f"z{z:02d}"
            _ = self.getvalue(wire)
            self.usage[z] = set([k for k in self.wires.keys() if not is_xy(k)])
            # and (z == 0 or k not in self.usage[                    len(self.usage)-1])

        self.zs = [self.wires[z] for z in reversed(zs)]

        return self.zs

    def find_segment(self) -> tuple[int, int] | None:

        diff = list(
            map(
                lambda pair: pair[0] == pair[1],
                zip(reversed(self.zs), reversed(self.expected)),
            )
        )
        diff1 = list(dropwhile(lambda p: p[1], enumerate(diff)))

        if diff1:
            diff2 = list(dropwhile(lambda p: not p[1], diff1))

            start_zindex = diff1[0][0]
            end_zindex = diff2[0][0]

            return start_zindex, end_zindex
        else:
            return None


    def find_candidates(self, zindex):
        w = f"z{zindex:02d}"
        candidates = []
        pending = [w]
        exp = self.expected[zindex]

        while pending:
            w = pending.pop(0)
            (i1, op, i2) = self.wire_setup[w]

            match (is_xy(i1), is_xy(i2)):
                case (True, True):
                    candidates.append(w)

                case _:
                    match (i1 in device.constants, i2 in device.constants):
                        case (True, True):
                            candidates.append(i1)
                            candidates.append(i2)

                        case (True, False):
                            candidates.append(i2)

            if exp:
                # constant or 0
                # 0 or constant
                pass

            else:
                # 0 or 0

                pass

            match (self.getvalue(i1), op, self.getvalue(i2)):
                case (1, "OR", 1):
                    raise Exception("Unexpected")

                case (1, "OR", 0):
                    pending.append(i1)

                case (0, "OR", 1):
                    pending.append(i2)

                case (0, "OR", 0):
                    pending.append(i2)

        return candidates

    def get_pairs(self):

        zeros = set([])
        ones = set([])

        match self.find_segment():
            case (start, end):

                if start == 0:
                    return [], []

                for i in range(start, end):

                    for z in range(0, start+1):
                        zw = f"z{z:02d}"
                        self.getvalue(zw)

                    xx = self.usage[i] - self.usage[i - 1]

                    for w in self.wire_setup.keys():

                        if w not in self.usage[i]:

                            if w not in self.wires:
                                self.getvalue(w)

                            if self.wires[w] == 0:
                                zeros.add(w)
                            else:
                                ones.add(w)

            case _:
                return None

        # print(zeros)
        # print(ones)

        return (start, end), (zeros, ones)

    def display(self):
        for i in range(45, -1, -1):
            print(f"{i: 3d}", end="")
        print()

        for x in [0] + self.xs:
            print(f"{x: 3d}", end="")
        print()

        for y in [0] + self.ys:
            print(f"{y: 3d}", end="")
        print()

        for e in self.expected:
            print(f"{e: 3d}", end="")
        print()

        diff = list(
            map(
                lambda pair: pair[0] == pair[1],
                zip(reversed(self.zs), reversed(self.expected)),
            )
        )

        for m in reversed(diff):
            print("   " if m else "  x", end="")
        print()

        for a in device.zs:
            print(f"{a: 3d}", end="")
        print()

    def solve(self):

        # self.display()
        # print('-' * 200)

        pairs = self.get_pairs()

        if pairs:

            ((start, end), (zeros, ones)) = pairs

            if len(self.swapped) == 8:
                return []

            for z in zeros:
                for o in ones:
                    if z not in self.swapped and o not in self.swapped:

                            cl = self.swap(z, o)
                            if not cl.is_recursive(z, o, set()):
                                print('Try swapping', z, 'for', o)
                                cl.run()

                                pairs1 = cl.get_pairs()

                                if pairs1:
                                    ((start1, end1), (zeros1, ones1)) = pairs1

                                    if start1 > start:
                                        print('Swapped', z, 'for', o)
                                        cl.display()
                                        print('-' * 200)

                                        swapped = cl.solve()

                                        if swapped and len(swapped) == 8:
                                            print('Hurray!')

                                    else:
                                        continue
                                else:
                                    print('Hurray!')
            return None

        else:
            if len(self.swapped) == 8:
                return self.swapped
            else:
                return None

    def find_anomaly(self, w: str):
        (i1, op, i2) = self.wire_setup[w]

        if i1[0] in ["x", "y"]:
            i1v = self.wires[i1]
            i2v = self.wires[i2]
        else:
            i1v = self.getvalue(i1)
            i2v = self.getvalue(i2)

        print(
            i1 + ("*" if i1 in self.constants else " "),
            i1v,
            f"{op:3s}",
            i2 + ("*" if i2 in self.constants else " "),
            i2v,
            "->",
            w,
            gate_output(op, i1v, i2v),
        )

        # print(i1 + ('*' if i1 in constants else ' '), f'{op:3s}', i2 + ('*' if i2 in constants else ' '), '->', w)

        if i1[0] not in ["x", "y"]:
            self.find_anomaly(i1)
        if i2[0] not in ["x", "y"]:
            self.find_anomaly(i2)


INPUT = PUZZLE_INPUT

def create_device(s: str) -> Device:

    ss: list[str] = s.split("\n\n")

    in_wires: dict[str, int] = {}
    in_wire_setup: dict[str, tuple[str, str, str]] = {}

    for line in ss[0].splitlines():
        wire, value = line.split(": ")
        in_wires[wire] = int(value)

    for line in ss[1].splitlines():
        [i1, op, i2, _, w] = line.split()
        in_wire_setup[w] = (i1, op, i2)

    return Device(in_wires, in_wire_setup, add)


def display_binary(xs, leading_zero=False):
    xx = reduce(lambda acc, d: acc * 10 + d, [x for x in xs])
    if leading_zero:
        print(0, xx, sep="")
    else:
        print(xx)


# display_binary(xs, True)
# display_binary(ys, True)
# display_binary(expected)
# display_binary(actual)

device = create_device(INPUT)
# device = device.swap('z07', 'pkm')
# ['qrw', 'kbg', 'hkj', 'rmq', 'vmv', 'cvq', 'pkm']

# device = device.swap('z07', 'pkm')
# device = device.swap('z07', 'cvq')
# device = device.swap('z07', 'vmv')
# device = device.swap('z07', 'rmq')
# device = device.swap('z07', 'hkj')
# device = device.swap('z07', 'kbg')
# device = device.swap('z07', 'qrw')

# device = device.swap('kfm', 'jbq')
decimal = get_decimal(device.run())
device.solve()


# print(decimal)

if decimal == device.expected_decimal:
    print("Done!")
else:
    for i in range(45, -1, -1):
        print(f"{i: 3d}", end="")
    print()

    for x in [0] + device.xs:
        print(f"{x: 3d}", end="")
    print()

    for y in [0] + device.ys:
        print(f"{y: 3d}", end="")
    print()

    for e in device.expected:
        print(f"{e: 3d}", end="")
    print()

    diff = list(
        map(
            lambda pair: pair[0] == pair[1],
            zip(reversed(device.zs), reversed(device.expected)),
        )
    )

    for match in reversed(diff):
        print("   " if match else "  x", end="")
    print()

    for a in device.zs:
        print(f"{a: 3d}", end="")
    print()
    #
    # print()
    # print("*** Anomaly Analysis ***")
    # print()
    #
    # for i, match in enumerate(diff):
    #     if not match:
    #         print("Anomaly for", "z" + f"{i:02d}")
    #         print()
    #         device.find_anomaly("z" + f"{i:02d}")
    #
    #         print()
    #         print()
