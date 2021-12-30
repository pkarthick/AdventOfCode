import os

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 3
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


def partition_by_bit(bits_list, pos):
    ones = []
    zeroes = []

    for bits in bits_list:
        if bits[pos] == "1":
            ones.append(bits)
        else:
            zeroes.append(bits)

    return (ones, zeroes)


def filter_by_bit_pos(bits_list, bit, pos):
    (ones, zeroes) = partition_by_bit(bits_list, pos)

    if bit == "1":
        return ones if len(ones) >= len(zeroes) else zeroes
    else:
        return ones if len(ones) < len(zeroes) else zeroes


def get_decimal_rating(bits):
    bit_count = len(bits)
    return sum([2 ** (bit_count - i - 1) for (i, b) in enumerate(bits) if b == "1"])


def get_rating(bits_list, bit):

    pos = 0
    while len(bits_list) > 1:
        bits_list = filter_by_bit_pos(bits_list, bit, pos)
        pos += 1

    return get_decimal_rating(bits_list[0])


def get_oxygen_generator_rating(bits_list):
    return get_rating(bits_list, "1")


def get_co2_scrubber_rating(bits_list):
    return get_rating(bits_list, "0")


def get_life_support_rating(input):

    lines = input.splitlines()

    bits_list = [lines[li] for li in range(0, len(lines))]

    o2_rating = get_oxygen_generator_rating(bits_list)
    co2_rating = get_co2_scrubber_rating(bits_list)

    return o2_rating * co2_rating


process_sample_input(get_life_support_rating)
process_puzzle_input(get_life_support_rating)
