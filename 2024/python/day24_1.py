from dataclasses import dataclass
from functools import reduce
from itertools import dropwhile
from typing import LiteralString

from data.day24 import PUZZLE_INPUT

# import sys
# sys.setrecursionlimit(10 ** 9)


@dataclass
class Device:
    wires: dict[str, int]
    wire_setup: dict[str, tuple[str, str, str]]

    def clone(self):
        wire_setup1 = {k: (v[0], v[1], v[2]) for k, v in wire_setup.items()}
        wires1 = {k: v for k, v in wires.items()}
        return Device(wires1, wire_setup1)

    def is_recursive(self, g1: str, g2: str) -> bool:
        (i1, _, i2) = self.wire_setup[g2]

        if i1 == g1 or i1 == g2 or i2 == g1 or i2 == g2:
            return True
        else:
            if i1[0] not in ["x", "y"] and i2[0] not in ["x", "y"]:
                return self.is_recursive(i1, g2) or self.is_recursive(i2, g2)
            else:
                return False

    def gate_output(self, gate: str, wire1: int, wire2: int) -> int:
        match gate:
            case "AND":
                return wire1 & wire2
            case "OR":
                return wire1 | wire2
            case "XOR":
                return wire1 ^ wire2
            case _:
                raise Exception(f"Unexpected gate: {gate}")

    def getvalue(self, wire: str):
        try:
            if wire in self.wires:
                return self.wires[wire]

            (inwire1, op, inwire2) = self.wire_setup[wire]

            match (inwire1 in wires, inwire2 in wires):
                case (True, True):
                    wires[wire] = self.gate_output(op, wires[inwire1], wires[inwire2])

                case (True, False):
                    wires[wire] = self.gate_output(
                        op, wires[inwire1], self.getvalue(inwire2)
                    )

                case (False, True):
                    wires[wire] = self.gate_output(
                        op, self.getvalue(inwire1), wires[inwire2]
                    )

                case (False, False):
                    wires[wire] = self.gate_output(
                        op, self.getvalue(inwire1), self.getvalue(inwire2)
                    )

        except Exception:
            pass

        return wires[wire]

    def show(self, w: str):
        for wire, wsetup in wire_setup.items():
            (xg, op, yg) = wsetup

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

        return cloned


INPUT = PUZZLE_INPUT

ss: list[LiteralString] = INPUT.split("\n\n")

wires: dict[str, int] = {}
wire_setup: dict[str, tuple[str, str, str]] = {}

for line in ss[0].splitlines():
    wire, value = line.split(": ")
    wires[wire] = int(value)

for line in ss[1].splitlines():
    [inwire1, op, inwire2, _, outwire] = line.split()
    wire_setup[outwire] = (inwire1, op, inwire2)

device = Device(wires, wire_setup)


xs = [wires[f"x{i:02d}"] for i in range(44, -1, -1)]
ys = [wires[f"y{i:02d}"] for i in range(44, -1, -1)]

constants: set[str] = set()

for wire, ws in wire_setup.items():
    (i1, op, i2) = ws
    if i1[0] in ["x", "y"] and i2[0] in ["x", "y"]:
        constants.add(wire)


def processs_derived_constants():
    while True:
        prev_len = len(constants)

        for wire, ws in wire_setup.items():
            (i1, op, i2) = ws
            if i1 in constants and i2 in constants:
                constants.add(wire)

        if len(constants) == prev_len:
            break


def get_variables(suffix):
    vars = []

    if suffix < 10:
        wire_suffix = "0" + str(suffix)
    else:
        wire_suffix = str(suffix)

    for wire, ws in wire_setup.items():
        (i1, op, i2) = ws
        if (i1 == "x" + wire_suffix or i1 == "y" + wire_suffix) and (
            i2 == "x" + wire_suffix or i2 == "y" + wire_suffix
        ):
            vars.append(wire)
            # print(i1, op, i2 , '->', wire)

    return vars


def traverse(vars):
    vars = set(vars)
    done = set()

    while vars:
        wire = vars.pop()
        done.add(wire)

        (i1, op, i2) = wire_setup[wire]

        print(i1, f"{op:3s}", i2, "->", wire)

        if i1[0] != "x" and i1[0] != "y" and i2[0] != "x" and i2[0] != "y":
            if i1 not in done:
                vars.add(i1)
            if i2 not in done:
                vars.add(i2)


# print(len(constants))
# processs_derived_constants()
# print(len(constants))

# initial_variables = get_variables(29)
# print(initial_variables)
# print()


def getdecimal(binaries):
    return reduce(lambda acc, d: acc * 2 + d, binaries)


def getbinary(zs):
    tot = 0

    for z in sorted(zs, reverse=True):
        tot = (tot * 2) + wires[z]

    return tot


xdecimal = getdecimal(xs)
ydecimal = getdecimal(ys)
totdecimal = xdecimal + ydecimal

print(totdecimal)

# print(totdecimal)
expected = str(bin(totdecimal))[2:]
expected = [ord(c) - 48 for c in expected]


zs = [f"z{i:02d}" for i in range(45, -1, -1)]


def run(wire_setup1, wires1):
    # for wire in wire_setup1.keys():
    #     if wire[0] != 'x' and wire[0] != 'y' and wire[0] != 'z':
    #         if wire in wires1:
    #             del wires1[wire]

    for wire in zs:
        _ = getvalue(wires1, wire)
        # print(wire, getvalue(wire))

    # print(getdecimal(actual))

    return [wires1[z] for z in zs]

    # return actual1


# actual = run(wire_setup, wires)

swap("z07", "pkm")
swap("kfm", "jbq")
swap("jsg", "std")

# swap('vwh', 'hcr')
# swap('jsg', 'vwp')
# swap('jsg', 'vwh')

# swap('trq', 'rrq')

# swap('vwh', 'vfd')

# swap('bsj', 'gjk')
# swap('bsj', 'vrm')
# swap('bsj', 'hth')

# swap('z35', 'trb')
# swap('tqr', 'vwh')
# swap('tqr', 'vtf')
# swap('tqr', 'jkq')

# swap('tqr', 'hth')

# swap('tqr', 'vrm')

# swap('z07', 'vfd')


actual = run(wire_setup, wires)


diff = list(
    map(lambda pair: pair[0] == pair[1], zip(reversed(actual), reversed(expected)))
)
diff1 = list(dropwhile(lambda p: p[1], enumerate(diff)))
diff2 = list(dropwhile(lambda p: not p[1], diff1))

start_zindex = diff1[0][0]
end_zindex = diff2[0][0]

print(diff1)


(problem_wire1, op, problem_wire2) = wire_setup[f"z{start_zindex:02d}"]

print(problem_wire1)
print(problem_wire2)

# if problem_wire1 in constants:
#     problem_wire = problem_wire1
# else:
#     problem_wire = problem_wire2

# problem_wire = 'pkm'

# wire_setup1 = {k:(v[0], v[1], v[2]) for k, v in wire_setup.items()}
# wires1 = {k:v for k, v in wires.items()}

# for wire in constants:


#     wire_setup2 = {k:(v[0], v[1], v[2]) for k, v in wire_setup.items()}

#     wires2 = {k:v for k, v in wires.items()}
#     # wires1 = wires.copy()
#     # wires2 = wires.copy()

#     swap(problem_wire, wire)
#     actual1 = run(wire_setup, wires)

#     d2 = getdecimal(actual)
#     diff = list(map(lambda pair: pair[0] == pair[1], zip(reversed(actual1), reversed(expected))))
#     xxs = list(dropwhile(lambda p: p[1], enumerate(diff)))

#     if xxs[0][0] >= end_zindex:
#         print(problem_wire, wire, d2, xxs)


#         # swap(problem_wire, wire)
#     wire_setup = {k:(v[0], v[1], v[2]) for k, v in wire_setup1.items()}
#     wires = {k:v for k, v in wires1.items()}


#     # excluded = ['bsj', 'z33', 'pcn', 'z39', 'z43', 'khb', 'wnj', 'rtt', 'dqt', 'hpj', 'mdm', 'smq', 'z35', 'z41', 'z31', 'qcm', 'mbg',
#     #             'z44', 'nvf', 'qbd', 'z37']

# excluded = []

# print(decimal)


# problem_wire = 'kws'

# for wire in wire_setup.keys():

#     if is_recursive(problem_wire, wire):
#         continue

#     try:

#         if wire not in excluded:

#             swap(problem_wire, wire)
#             act = run()
#             swap(problem_wire, wire)

#             d2 = getdecimal(act)
#             diff = list(map(lambda pair: pair[0] == pair[1], zip(reversed(act), reversed(expected))))
#             xxs = list(dropwhile(lambda p: p[1], enumerate(diff)))

#             if xxs[0][0] > 35:
#                 print(problem_wire, wire, d2, xxs)

#             decimal = getdecimal(act)
#             swap(problem_wire, wire)

#             if decimal == totdecimal:
#                 print('Hurrah!')

#     except:
#         pass

#     finally:
#         pass


# print()
# print(tot)


def display_binary(xs, leadingzero=False):
    xx = reduce(lambda acc, d: acc * 10 + d, [x for x in xs])
    if leadingzero:
        print(0, xx, sep="")
    else:
        print(xx)


def find_anomaly(w):
    (i1, op, i2) = wire_setup[w]

    i1v = None
    i2v = None

    if i1[0] in ["x", "y"]:
        i1v = wires[i1]
        i2v = wires[i2]
    else:
        i1v = getvalue(wires, i1)
        i2v = getvalue(wires, i2)

    print(
        i1 + ("*" if i1 in constants else " "),
        i1v,
        f"{op:3s}",
        i2 + ("*" if i2 in constants else " "),
        i2v,
        "->",
        w,
        gate_output(op, i1v, i2v),
    )

    # print(i1 + ('*' if i1 in constants else ' '), f'{op:3s}', i2 + ('*' if i2 in constants else ' '), '->', w)

    # if i1[0] not in ['x', 'y']:
    #     find_anomaly(i1)
    # if i2[0] not in ['x', 'y']:
    #     find_anomaly(i2)


# display_binary(xs, True)
# display_binary(ys, True)
# display_binary(expected)
# display_binary(actual)


decimal = getdecimal(actual)
# print(decimal)

if decimal == totdecimal:
    print("Done!")
else:
    for i in range(45, -1, -1):
        print(f"{i: 3d}", end="")
    print()

    for x in [0] + xs:
        print(f"{x: 3d}", end="")
    print()

    for y in [0] + ys:
        print(f"{y: 3d}", end="")
    print()

    for e in expected:
        print(f"{e: 3d}", end="")
    print()

    diff = list(
        map(lambda pair: pair[0] == pair[1], zip(reversed(actual), reversed(expected)))
    )

    for match in reversed(diff):
        print("   " if match else "  x", end="")
    print()

    for a in actual:
        print(f"{a: 3d}", end="")
    print()

    print()
    print("*** Anamoly Analysis ***")
    print()

    for i, match in enumerate(diff):
        if not match:
            print("Anamoly for", "z" + f"{i:02d}")
            print()
            find_anomaly("z" + f"{i:02d}")

            print()
            print()
