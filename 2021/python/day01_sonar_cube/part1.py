import os

TEST_DATA_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)).rsplit('/python/', 1)[0], 'testdata')

day = 1
part = 1

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

def get_increase_count():
  return len(list(filter(lambda x: int(x[0]) < int(x[1]), zip(lines, lines[1:]))))

actual = get_increase_count()

print(f'{kind} output:')
print(actual)

assert(actual == expected)

kind = "puzzle"

input = get_input(kind)
expected = int(get_output(kind))
lines = input.splitlines()
actual = get_increase_count()

print(f'{kind} output:')
print(actual)

assert(actual == expected)
  
