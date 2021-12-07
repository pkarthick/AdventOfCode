import os
import operator
from itertools import accumulate
from functools import reduce

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 7
part = 2

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


def calculate_required_fuel(input):

    crab_positions = list(map(int, input.split(",")))

    max_pos = max(crab_positions)

    costs = list(accumulate(range(0, max_pos + 1)))

    fuel = min(
        map(
            lambda new_pos: reduce(
                operator.add, map(lambda pos: costs[abs(pos - new_pos)], crab_positions)
            ),
            range(0, max_pos + 1),
        )
    )

    return fuel


process_sample_input(calculate_required_fuel)
process_puzzle_input(calculate_required_fuel)
