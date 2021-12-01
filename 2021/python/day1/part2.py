day = 1
part = 2

def getInput(kind):
  with open(f'../testdata/{day}/{kind}_{part}.in') as fo:
    return fo.read()

def getOutput(kind):
  with open(f'../testdata/{day}/{kind}_{part}.out') as fo:
    return fo.read()

kind = "sample"

input = getInput(kind)
expected = int(getOutput(kind))

lines = input.splitlines()

def get_depths(lines):
  return list(map(int, lines))

def get_increase_count(depths):
  return len(list(filter(lambda x: x[0] < x[1], zip(depths, depths[1:]))))

depths = get_depths(lines)
depths = list(map(sum, zip(depths, depths[1:], depths[2:])))
actual = get_increase_count(depths)

print(f'{kind} Output:')
print(actual)

assert(actual == expected)

kind = "puzzle"

input = getInput(kind)
expected = int(getOutput(kind))

lines = input.splitlines()

depths = get_depths(lines)
depths = list(map(sum, zip(depths, depths[1:], depths[2:])))
actual = get_increase_count(depths)

print(f'{kind} Output:')
print(actual)

assert(actual == expected)
