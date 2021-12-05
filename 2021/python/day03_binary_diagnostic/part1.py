import os

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 3
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


def get_decimal_rating(bits, bit):
    bit_count = len(bits)
    return sum([2 ** (bit_count - i - 1) for (i, b) in enumerate(bits) if b == bit])


def get_power_consumption(input):

    lines = input.splitlines()

    total = len(lines)

    bit_count = len(lines[0])

    final_bits = [
        count > total - count
        for count in [
            sum([int(lines[li][bi]) for li in range(0, len(lines))])
            for bi in range(0, bit_count)
        ]
    ]

    gamma = get_decimal_rating(final_bits, True)
    epsilon = get_decimal_rating(final_bits, False)

    return gamma * epsilon


process_sample_input(get_power_consumption)
process_puzzle_input(get_power_consumption)
