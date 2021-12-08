import os
from itertools import filterfalse
from functools import reduce
import operator

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 8
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


def sum_of_output_values(input):

    line_segments = list(
        map(
            lambda l: list(map(lambda s: s.split(" "), l.split(" | "))),
            input.splitlines(),
        )
    )

    segments_pairs = list(map(lambda xs: (xs[0], xs[1]), line_segments))

    total = 0

    for (in_segs, out_segs) in segments_pairs:

        in_segs = list(map(set, in_segs))
        in_segs.sort(key=lambda xs: len(xs))

        one = in_segs[0]
        seven = in_segs[1]
        four = in_segs[2]
        eight = in_segs[9]

        nine = next(filter(lambda seg: four < seg, in_segs[6:9]))
        zero_and_six = list(filterfalse(lambda seg: four < seg, in_segs[6:9]))

        zero = next(filter(lambda seg: one < seg, zero_and_six))
        six = next(filterfalse(lambda seg: one < seg, zero_and_six))

        three = next(filter(lambda seg: one < seg, in_segs[3:6]))
        two_and_five = list(filter(lambda seg: seg != three, in_segs[3:6]))
        five = next(filter(lambda seg: four | seg == nine, two_and_five))
        two = next(filter(lambda seg: four | seg == eight, two_and_five))

        all_digits = [zero, one, two, three, four, five, six, seven, eight, nine]

        pows = [1000, 100, 10, 1]

        for powi, out_seg in enumerate(map(set, out_segs)):

            for digit, digit_set in enumerate(all_digits):

                if out_seg == digit_set:
                    total += digit * pows[powi]
                    break

    return total


process_sample_input(sum_of_output_values)
process_puzzle_input(sum_of_output_values)
