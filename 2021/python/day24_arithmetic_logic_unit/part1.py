import functools
import os
from typing import Counter
from functools import lru_cache
import itertools

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 24
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


def compute_prog(programs, pi, z, digit):


  for digit in range(9, 0, -1):
    z = execute_program(programs[pi], z, digit)

    if pi < 13:
      compute_prog(programs, pi+1, z, digit)
    else:
      if z == 0:
        print("Found IT!")
        pass
      


def compute_program(program):

  vars = {'w': 0, 'x': 0, 'y': 0, 'z': 0}

  mods = [12, 11, 13, 11, 14, -10, 11, -9, -3, 13, -5, -10, -4, -5]

  digits = 9

  lines = program.splitlines()

  program_length = len(lines) // 14

  programs = [lines[i : i+18] for i in range(0, len(lines), program_length)]

  zs = set([0])

  digits = [7,1]

  

  for pi in range(0, 14):

      print('pi', pi)
      program = programs[pi]

      zs1 = set([])

      if pi < len(digits):
        ds = [digits[pi]]
      else:
        ds = list(range(9,0,-1))

      for z in zs:
        
        for d in ds:
          z1 = execute_program(program, z, d)

          if pi == 13 and z1 == 0:
            pass


          zs1.add(z1)


      zs = zs1

  # print(zs)

  return 10

def execute_program(program, z, digit):
    vars = {'w': 0, 'x': 0, 'y': 0, 'z': z}

    for line in program:
      match (line.split(' ')):
        case ['inp', v]:
          vars[v] = digit
        case ['add', a, b]:
          if b in ['w', 'x', 'y', 'z']:
            vars[a] = vars[a] + vars[b]
          else:
            vars[a] = vars[a] + int(b)
        case ['mul', a, b]:
          if b in ['w', 'x', 'y', 'z']:
            vars[a] = vars[a] * vars[b]
          else:
            vars[a] = vars[a] * int(b)

        case ['div', a, b]:
          if b in ['w', 'x', 'y', 'z']:
            vars[a] = vars[a] // vars[b]
          else:
            vars[a] = vars[a] // int(b)
        case ['mod', a, b]:
          if b in ['w', 'x', 'y', 'z']:
            vars[a] = vars[a] % vars[b]
          else:
            vars[a] = vars[a] % int(b)
        case ['eql', 'x', '0']:
            vars['x'] = 1 if vars['x'] == 0 else 0
        case ['eql', 'x', 'w']:
            if vars['x'] == vars['w']:
              vars['x'] = 1
              print('digit', digit)
            else:
              vars['x'] = 0

        case ['eql', a, b]:
          if b in ['w', 'x', 'y', 'z']:
            vars[a] = 1 if vars[a] == vars[b] else 0
          else:
            vars[a] = 1 if vars[a] == int(b) else 0

        case _:
          raise BaseException(line)

    if vars['z'] == 0:
      print(digit)

    return vars['z']

process_sample_input(compute_program)
# process_puzzle_input(compute_program)
