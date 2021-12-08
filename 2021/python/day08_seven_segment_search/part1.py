import os
from itertools import groupby
from functools import reduce
import operator

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 8
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


def find_unique_digits(input):

    unique_digits_lengths = [2, 4, 3, 7]

    output_segments = list(
        map(lambda l: l.split(" | ")[1].split(" "), input.splitlines())
    )

    unique_segments = list(
        map(
            lambda segments: list(
                filter(lambda segment: len(segment) in unique_digits_lengths, segments)
            ),
            output_segments,
        )
    )

    unique_segments_count = reduce(operator.add, map(len, unique_segments))
    return unique_segments_count


process_sample_input(find_unique_digits)
process_puzzle_input(find_unique_digits)
