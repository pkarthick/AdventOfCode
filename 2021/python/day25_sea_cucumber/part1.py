import functools
import os
from typing import Counter
from functools import lru_cache
import itertools

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 25
part = 1

INPUT_KIND_SAMPLE = "sample"
INPUT_KIND_PUZZLE = "puzzle"


def get_input(kind):
    file_path = f"{TEST_DATA_DIR}/{day}/{kind}_{part}.in"
    with open(file_path) as fo:
        return fo.read()


def get_output(kind):
    file_path = f"{TEST_DATA_DIR}/{day}/{kind}_{part}.out"
    with open(file_path) as fo:
        return fo.read()


def process_input(input_kind, process_fn):

    input = get_input(input_kind)
    actual = process_fn(input)

    if input_kind == "sample":
        expected = int(get_output(input_kind))
        assert actual == expected

    print(f"{input_kind} output:")
    print(actual)


def process_sample_input(process_fn):
    process_input(INPUT_KIND_SAMPLE, process_fn)


def process_puzzle_input(process_fn):
    process_input(INPUT_KIND_PUZZLE, process_fn)

def move_east(cucumbers, rc, cc):

  has_moved = False
  moved_cucumbers = [[c for c in r] for r in cucumbers]

  for r in range(rc-1, -1, -1):
    for c in range(cc-1, -1, -1):

      c1 = 0 if c == cc-1 else c+1

      match (cucumbers[r][c], cucumbers[r][c1]):
        case ('>', '.'):
          has_moved = True
          moved_cucumbers[r][c], moved_cucumbers[r][c1] = '.', '>'
        case _:
          pass

  return has_moved, moved_cucumbers

def move_south(cucumbers, rc, cc):

  has_moved = False

  moved_cucumbers = [[c for c in r] for r in cucumbers]

  for c in range(cc-1, -1, -1):
    for r in range(rc-1, -1, -1):
      
      r1 = 0 if r == rc-1 else r+1

      match (cucumbers[r][c], cucumbers[r1][c]):
        case ('v', '.'):
          has_moved = True
          moved_cucumbers[r][c], moved_cucumbers[r1][c] = '.', 'v'
        case _:
          pass

  return has_moved, moved_cucumbers

def layout(cucumbers, steps):
  
  print(f'After step {steps}')
  print()
  
  for row in cucumbers:
    for c in row:
      print(c, end='')
    print()

  print()
  print()

def step_when_cucumbers_stop_moving(input):

  lines = input.splitlines()

  cucumbers = [[c for c in l] for l in lines]


  rc = len(cucumbers)
  cc = len(cucumbers[0])

  steps = 0

  east, south = True, True

  while east or south:

    east, cucumbers = move_east(cucumbers, rc, cc)
    south, cucumbers = move_south(cucumbers, rc, cc)

    steps += 1

  return steps


process_sample_input(step_when_cucumbers_stop_moving)
process_puzzle_input(step_when_cucumbers_stop_moving)
