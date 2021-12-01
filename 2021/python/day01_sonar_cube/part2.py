import os

TEST_DATA_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)).rsplit('/python/', 1)[0], 'testdata')

day = 1
part = 2

def get_input(kind):

  file_path = f'{TEST_DATA_DIR}/{day}/{kind}_{part}.in'

  with open(file_path) as fo:
    return fo.read()

def get_output(kind):

  file_path = f'{TEST_DATA_DIR}/{day}/{kind}_{part}.out'

  with open(file_path) as fo:
    return fo.read()


kind = "sample"

input = get_input(kind)
expected = int(get_output(kind))

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

input = get_input(kind)
expected = int(get_output(kind))

lines = input.splitlines()

depths = get_depths(lines)
depths = list(map(sum, zip(depths, depths[1:], depths[2:])))
actual = get_increase_count(depths)

print(f'{kind} Output:')
print(actual)

assert(actual == expected)
