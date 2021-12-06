import os

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 6
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


def count_lantern_fish(input):
    
    ages = sorted(map(int, input.split(',')))

    age_groups = {age:0 for age in range(0,9)}

    for age in ages:
        age_groups[age] += 1
    
    for _ in range(0,80):
        
        new_count = age_groups[0]

        for age in range(1, 9):
            age_groups[age-1] = age_groups[age]

        age_groups[8] = new_count # add new 8s
        age_groups[6] = age_groups[6] + new_count #convert 0s to 6s
        
    return sum(age_groups.values())


process_sample_input(count_lantern_fish)
process_puzzle_input(count_lantern_fish)
