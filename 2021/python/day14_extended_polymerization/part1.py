import os
from typing import Counter
from functools import lru_cache
import itertools

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 14
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




def get_diff_between_most_and_least(input):

  lines = input.splitlines()
  polymer = lines[0]
  
  rules = dict(map(lambda arr: (arr[0], arr[0][0] + arr[1]) , map(lambda l: l.split(' -> '), lines[2:])))

  cache = {}

  pairs = list(map(lambda kv: ((kv, 1), rules[kv]+kv[1]), rules))

  cache.update(pairs)

  def extend_polymer(s, times):

    if (s, times) in cache:
      return cache[(s, times)]
    else:

      if times == 1:

        cache[(s,0)] = s

        if len(s) == 2:
          return cache[(s,1)]
        else:

          if (s,1) not in cache:

            res = []
            prev = cache[(s[0:times*2],1)]

            for i in range(times*2, len(s), times*2):
              # if (s[i: 2], 1) not in cache:

              prev = prev[:-1] + cache[(s[i-1:i+1],1)][:-1] + cache[(s[i:i+times*2],1)]
              cache[(s[0:i+times*2],1)] = prev



            # for i in range(3, len(s)+1):

            #   if (s[0: i], 1) not in cache:

            #     prev = cache[(s[0: i-1], 1)][:-1]

            #     val = cache[(s[i-2 : i], 1)]

            #     cache[(s[0: i], 1)] = prev + val

              
          if (s,1) not in cache:
            raise "Unexpected scenario, was expecting in cache!"


          

          return cache[(s,1)]
        

      else:
        
        polymer = extend_polymer(s, times-1)
        cache[(s,times-1)] = polymer
        cache[(s,times)] = extend_polymer(polymer, 1)
        return cache[(s,times)]


  times = 20

  # for key in rules:
  #   extend_polymer(key, 20)
  #   extend_polymer(rules[key], 20)

  ex_polymer = extend_polymer(polymer, 20)
  # print(polymer)
  # print(ex_polymer)
  print(Counter(ex_polymer))

  

  return 10


process_sample_input(get_diff_between_most_and_least)
process_puzzle_input(get_diff_between_most_and_least)
