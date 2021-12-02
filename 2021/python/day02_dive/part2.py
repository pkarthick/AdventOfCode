import os
from functools import reduce

TEST_DATA_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)).rsplit('/python/', 1)[0], 'testdata')

day = 2
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

def get_instruction(l):
  ls = l.split()
  return ls[0], int(ls[1])

def process_instruction(dwa, ins):
  d, w, a = dwa
  match ins:
    case ['forward', dis]:
      return d + a * dis, w+dis, a
    case ['up', dis]:
      return d, w, a - dis
    case ['down', dis]:
      return d, w, a + dis

lines = input.splitlines()

(d, w, _) = reduce(process_instruction, map(get_instruction, lines), (0,0,0))

actual = d * w

print(f'{kind} output:')
print(actual)

assert(actual == expected)

kind = "puzzle"

input = get_input(kind)
lines = input.splitlines()

(d, w, _) = reduce(process_instruction, map(get_instruction, lines), (0,0,0))

actual = d * w

print(f'{kind} output:')
print(actual)

