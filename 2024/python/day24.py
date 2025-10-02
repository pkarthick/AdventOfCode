from dataclasses import dataclass
from functools import reduce
from itertools import dropwhile, permutations
from operator import add
from typing import Callable

from data.day24 import PUZZLE_INPUT


def get_decimal(binaries: list[int]):
    if binaries:
        return reduce(lambda acc, d: acc * 2 + d, binaries)

    return -1

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
    wires: dict[str, int]
    wire_setup: dict[str, tuple[str, str, str]]
    constants: dict[str, int]
    xs: list[int]
    ys: list[int]
    zs: list[int]
    expected: list[int]
    operation: Callable[[int, int], int]
    usage: dict[int, set[str]]

    def __init__(
        self,
        constants: dict[str, int],
        wire_setup: dict[str, tuple[str, str, str]],
        operation: Callable[[int, int], int],
    ):
        self.wires = {}

        self.wire_setup = wire_setup
        self.operation = operation
        self.usage = {}
        self.swapped = set()
        self.constants = constants

        self.zeroes = set()
        self.ones = set()
        self.start = 0
       
        for wire, ws in wire_setup.items():
            (i1, _, i2) = ws
            if i1[0] in ["x", "y"] and i2[0] in ["x", "y"]:
                val = self.getvalue(wire)
                self.constants[wire] = self.getvalue(wire)
                if val == 1:
                    self.ones.add(wire)
                else:
                    self.zeroes.add(wire)
                

        self.xs = [self.constants[f"x{x:02d}"] for x in range(44, -1, -1)]
        self.ys = [self.constants[f"y{y:02d}"] for y in range(44, -1, -1)]
        self.zs = []

        x_decimal = get_decimal(self.xs)
        y_decimal = get_decimal(self.ys)
        self.expected_decimal = operation(x_decimal, y_decimal)

        expected = str(bin(self.expected_decimal))[2:]
        self.expected = [ord(c) - 48 for c in expected]

    def clone(self):
        wire_setup1 = {k: (v[0], v[1], v[2]) for k, v in self.wire_setup.items()}
        constants1 = {k: v for k, v in self.constants.items()}
        device = Device(constants1, wire_setup1, self.operation)
        device.swapped = set(self.swapped)
        device.start = self.start
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

        if w in self.constants:
            # self.wires[w] = self.constants[w]
            return self.constants[w]

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

    def reset(self, w: str):

        if is_xy(w):
            return
        
        (i1, _, i2) = self.wire_setup[w]

        if w in self.wires:
            del self.wires[w]
            self.reset(i1)
            self.reset(i2)

    def swap(self, w1: str, w2: str):

        cloned = self.clone()

        cloned.usage.clear()

        if w1 in cloned.constants and w2 in cloned.constants:
            cloned.constants[w1], cloned.constants[w2] = cloned.constants[w2], cloned.constants[w1]
    
        (x1, xop, x2) = self.wire_setup[w1]
        (y1, yop, y2) = self.wire_setup[w2]
        cloned.wire_setup[w1] = (y1, yop, y2)
        cloned.wire_setup[w2] = (x1, xop, x2)

        cloned.reset(w1)
        cloned.reset(w2)

        cloned.swapped.add(w1)
        cloned.swapped.add(w2)

        return cloned

    def get_binary(self):
        return reduce(lambda acc, z: acc * 2 + z, self.zs, 0)

    def run(self, zt: int):

        # self.wires = {k:v for k, v in self.wires.items() if is_xy(k) }

        zs = [f"z{z:02d}" for z in range(zt+1)]

        for z in range(zt+1):
            wire = f"z{z:02d}"
            _ = self.getvalue(wire)
            self.usage[z] = set([k for k in self.wires.keys() if not is_xy(k)]) # - set([wire])
            # and (z == 0 or k not in self.usage[                    len(self.usage)-1])

        self.zs = [self.getvalue(z1) for z1 in reversed(zs)]

        return self.zs

    def find_segment(self) -> int | None:

        diff = list(
            map(
                lambda pair: pair[0] == pair[1],
                zip(reversed(self.zs), reversed(self.expected)),
            )
        )

        diff1 = list(dropwhile(lambda p: p[1], enumerate(diff)))

        if diff1:
            return diff1[0][0]
        else:
            return None


    def find_candidates1(self, zindex):
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

    def try_candidates(self, candidates, zw, zt):
        cls = []
        for w in candidates:
            if w not in self.swapped:
                cl = self.try_swap(zw, w, zt)
                if cl is not None and cl.start > zt:
                    cls.append(cl)

        return cls


    def try_swap(self, zw, w, zt):

        cl = self.swap(zw, w)
        if not cl.is_recursive(zw, w, set()):
            print('Try swapping', zw, 'for', w)
            cl.run(45)

            start1 = cl.find_segment()

            if start1 is None:
                cl.start = 45
                return cl

            if start1 > cl.start:
                cl.start = start1
                return cl

            # if start1 is None or start1 > zt:
                
            #     cl.start = zt + 1
                
            #     print(' ... Done!')
            #     # cl.display()
            #     # print('-' * 200)

            #     swapped = cl.solve()

            #     if swapped and len(swapped) == 8:
            #         print('Hurray!')
            # else:
            #     print(' ... Failed!')

        return None

    def solve_swap(self, start, zw, zt):

        exp = self.expected[start]

        if exp == 0:
            cls = self.try_candidates(self.zeroes, zw, start)
        else:
            cls = self.try_candidates(self.ones, zw, start)

        sorted_cls = sorted(cls, key=lambda cl: cl.start, reverse=True)

        for cl in sorted_cls:
            if cl.solve() is not None:
                print()
            else:
                print()

        print()


    def find_candidates(self, w: str, candidates: list[str]):

        (i1, op, i2) = self.wire_setup[w]
        i1v = self.getvalue(i1)
        i2v = self.getvalue(i2)

        if i1 in self.constants and i2 in self.constants:
            candidates.append(i1)
            candidates.append(i2)
            return candidates

        match ((i1v, i1 in self.constants), op, (i2v, i2 in self.constants)):

            case ((_, False), _, (_, False)):
                self.find_candidates(i1, candidates)
                self.find_candidates(i2, candidates)
                return candidates

            case ((_, True), _, (_, True)):
                candidates.append(i1)
                candidates.append(i2)
                candidates.append(w)
                return candidates
            
            case ((0, True), 'XOR', (0, False)):
                candidates.append(i1)
                return self.find_candidates(i2, candidates)

            case ((0, False), 'XOR', (0, True)):
                candidates.append(i2)
                return self.find_candidates(i1, candidates)

            case ((0, False), 'XOR', (1, True)):
                candidates.append(i2)
                return self.find_candidates(i1, candidates)

            case ((0, True), 'XOR', (1, False)):
                candidates.append(i1)
                return self.find_candidates(i2, candidates)
            
            case (_, 'XOR', _):
                candidates.append(i1)
                candidates.append(i2)
                candidates.append(w)
                return candidates

            case ((0, False), 'OR', (0, True)):
                candidates.append(i2)
                return self.find_candidates(i1, candidates)

            case ((0, True), 'OR', (0, False)):
                candidates.append(i1)
                return self.find_candidates(i2, candidates)

            case ((0, True), 'OR', (1, False)):
                candidates.append(i1)
                return self.find_candidates(i2, candidates)

            case ((1, False), 'OR', (0, True)):
                candidates.append(i2)
                return self.find_candidates(i1, candidates)

            case (_, 'OR', _):
                candidates.append(i1)
                candidates.append(i2)
                candidates.append(w)
                return candidates

            case ((1, True), 'AND', (_, False)):
                candidates.append(i1)
                return self.find_candidates(i2, candidates)

            case ((_, False), 'AND', (1, True)):
                candidates.append(i2)
                return self.find_candidates(i1, candidates)

            case ((0, True), 'AND', (1, False)):
                candidates.append(i1)
                return self.find_candidates(i2, candidates)
            
            case ((1, False), 'AND', (0, True)):
                candidates.append(i2)
                return self.find_candidates(i1, candidates)

            case (_, 'AND', _):
                candidates.append(i1)
                candidates.append(i2)
                candidates.append(w)
                return candidates
            
            case _:
                print()


    def solve(self):

        # self.display()
        # print('-' * 200)

        # for zt in range(self.start, 46):

        self.run(45)

        start = self.find_segment()

        if start is not None:

            if len(self.swapped) == 8:
                return []
            
            zw = f'z{start:02d}'

            (i1, op, i2) = self.wire_setup[zw]

            if is_xy(i1) and is_xy(i2):
                
                self.solve_swap(start, zw, None)

                # if exp == 0:
                #     for w in self.zeroes:
                #         if w not in self.swapped:
                #             # if w not in self.usage[start-1]:
                #                 cl = self.swap(zw, w)
                #                 if not cl.is_recursive(zw, w, set()):
                #                     print('Try swapping', zw, 'for', w)
                #                     cl.run(zt)

                #                     start1 = cl.find_segment(zt)

                #                     if start1 is None:
                                        
                #                         cl.start = zt + 1
                                        
                #                         print('Swapping', zw, 'for', w)
                #                         # cl.display()
                #                         # print('-' * 200)

                #                         swapped = cl.solve()

                #                         if swapped and len(swapped) == 8:
                #                             print('Hurray!')

                # else:
                #     for w in self.ones:
                #         if w not in self.swapped:
                #             cl = self.swap(zw, w)
                #             if not cl.is_recursive(zw, w, set()):
                #                 print('Try swapping', zw, 'for', w)
                #                 cl.run()

                #                 start1 = cl.find_segment(zt)

                #                 if not start1:

                #                     print('Swapping', zw, 'for', w)

                #                     cl.start = zt + 1

                #                     # cl.display()
                #                     # print('-' * 200)

                #                     swapped = cl.solve()

                #                     if swapped and len(swapped) == 8:
                #                         print('Hurray!')
            
            
            else:

                # (i1, op, i2) = self.wire_setup[zw]

                candidates = []
                # self.find_candidates(zw, candidates)
                mzw = i1

                if i1 in self.constants:
                    mzw = i2
                    candidates = []
                    self.find_candidates(i2, candidates)
                else:
                    mzw = i1
                    candidates = []
                    self.find_candidates(i1, candidates)

                for candidate in set(candidates):

                    # swp = self.swap(mzw, candidate)

                    cl = self.try_swap(mzw, candidate, None)

                    if cl is not None and cl.start > self.start:

                        if len(self.swapped) == 8:
                            print('')
                        else:
                            print('')

                print(candidates)
                

        
        # else:
        #     self.start = zt

        if len(self.swapped) == 8:
            return self.swapped
        else:
            return None

    def find_anomaly(self, w: str):
        (i1, op, i2) = self.wire_setup[w]

        if i1[0] in ["x", "y"]:
            i1v = self.constants[i1]
            i2v = self.constants[i2]
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

    in_wire_setup: dict[str, tuple[str, str, str]] = {}
    constants: dict[str, int] = {}

    for line in ss[0].splitlines():
        wire, value = line.split(": ")
        constants[wire] = int(value)

    for line in ss[1].splitlines():
        [i1, op, i2, _, w] = line.split()
        in_wire_setup[w] = (i1, op, i2)

    return Device(constants, in_wire_setup, add)


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

# device.run(6)

# print('pkm' in device.constants)
# print('z07' in device.constants)

# just_constants = [c for c in device.constants if not is_xy(c)]

# eights = list(permutations(just_constants, 8))

# print(len(eights))

# device = device.swap('tqr', 'vtf')

# device = device.swap('hth', 'tqr')
# device = device.swap('z07', 'pkm')
# device = device.swap('kfm', 'jbq')

# device = device.swap('rvr', 'smq')

# device = device.swap('pcc', 'sbm')

# device = device.swap('std', 'smq')


# 25 ['rvr', 'tgf', 'z26']
# 26 ['mcw', 'sbm', 'z27']
# 27 ['jsg', 'wvv', 'z28']
# 28 ['hnv', 'std', 'z29']
# 29 ['mdm', 'smq', 'z30']
# 30 ['dhj', 'pcn', 'z31']

# 26 ['mcw', 'sbm', 'z27']
# 27 ['jsg', 'wvv', 'z28']

# device = device.swap('wvv', 'mcw')

# ['qrw', 'kbg', 'hkj', 'rmq', 'vmv', 'cvq', 'pkm']

# device = device.swap('z07', 'pkm')
# device = device.swap('z07', 'pkm')
# device = device.swap('z07', 'vmv')
# device = device.swap('z07', 'rmq')
# device = device.swap('z07', 'hkj')
# device = device.swap('z07', 'kbg')
# device = device.swap('z07', 'qrw')

# device = device.swap('dtw', 'gvv')
# device = device.swap('dtw', 'smg')
# device = device.swap('dtw', 'mbk')

# device = device.swap('hds', 'bqn')
# device = device.swap('hds', 'tcq')

# device = device.swap('hcr', 'hnv')

# device = device.swap('sbm', 'tqr')


# device = device.swap('vfd', 'vwh')

# device = device.swap('gvv', 'bsj')

# device = device.swap('vfd', 'rrq')

# device = device.swap('vbc', 'nws')

# device = device.swap('bsj', 'nws')
# device = device.swap('bsj', 'jnb')


# decimal = get_decimal(device.run())

device.run(45)
device.solve()

# for k in range(1, 36):
#     print(k, [(x, device.getvalue(x)) for x in sorted(device.usage[k] - device.usage[k-1])])

# u = set()
# i = set(device.usage[45])

# for k in range(44, -1, -1):
#     u.add(f'x{k:02d}')

# for k in range(44, -1, -1):
#     u.add(f'y{k:02d}')

# for k in range(0, 45):

#     print(k, sorted(device.usage[k+1].symmetric_difference(device.usage[k])))

    # u = u.union(device.usage[k].symmetric_difference(device.usage[k+1]))
    # i = i & device.usage[k]
    # print(k, sorted(u))
    # print(k, sorted(device.usage[k] - device.usage[k-1]))

# print(len(i))
# print(sorted(i))

# for k in range(10, 6, -1):
#     print(k, sorted(i.symmetric_difference(device.usage[k])))

# i = device.usage[8]
# # print('8 - 7', i)

# for k in range(8, 10):
#     i = i & device.usage[k]

# print(i - device.usage[6])


# print(sorted(u))
# print(sorted(set(device.wire_setup.keys())))

# print(len(device.usage[7]))

# print('pkm' in device.usage[0])

# for kk in range(7, 11):
#     print('pkm' in device.usage[kk] - device.usage[0])

# missing = set(device.wire_setup.keys()) - u


# print(device.usage[7] & missing)

# print(len(missing))
# print(sorted(missing))


# print(len(u))

# print(len(device.wire_setup.keys()))
# print(set(device.wire_setup.keys()))
# print(set(device.wire_setup.keys()) - u)


# print(len(u - set(device.wire_setup.keys())))
# print(len(set(device.wire_setup.keys()) - u))


# device.solve()

decimal = get_decimal(device.zs)

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
    
    # print()
    # print("*** Anomaly Analysis ***")
    # print()
    
    # for i, match in enumerate(diff):
    #     if not match:
    #         print("Anomaly for", "z" + f"{i:02d}")
    #         print()
    #         device.find_anomaly("z" + f"{i:02d}")
    
    #         print()
    #         print()
