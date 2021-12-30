import os
from itertools import groupby
from functools import reduce
import operator

TEST_DATA_DIR = os.path.join(
    os.path.dirname(os.path.realpath(__file__)).rsplit("/python/", 1)[0], "testdata"
)

day = 18
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

class S:
    def __str__(self) -> str:
        return '['
   
    def __repr__(self) -> str:
        return '['

class E:

    def __str__(self) -> str:
        return ']'
    
    def __repr__(self) -> str:
        return ']'

class C:
    
    def __str__(self) -> str:
        return ','

    
    def __repr__(self) -> str:
        return ','


class Number:
    def __init__(self, s):
        super(Number, self).__init__()
        self.tokens = [get_token(c) for c in s]

    def __repr__(self):
        return "".join(map(repr, self.tokens))

    def reduce(self):

        level = 0
        left_index = None
        i = 0
        count = len(self.tokens)

        while True:

            if i == len(self.tokens):
                if count > len(self.tokens):
                    i = 0
                    continue
                else:
                    break

            match self.tokens[i]:
                case S():

                    level += 1

                    if level == 4:

                        match list(self.tokens[i : i+5]):
                            case [S(), x, C(), y, E()]:

                                for right_index in range(i+5, len(self.tokens)):
                                    match self.tokens[right_index]:
                                        case int(x):
                                            break
                                        case _:
                                            pass

                                if left_index == None:
                                    self.tokens[right_index] = self.tokens[right_index] + y
                                    self.tokens[i: i+5] = [0]
                                else:
                                    self.tokens[i: i+5] = ['<', '>']


                            case _:
                                pass


                case E():
                    level -= 1

                case C():
                    pass

                case int(x):
                    left_index = i

            
            i += 1


def get_token(ch):
    match ch:
        case '[': 
            return S()
        case ']': 
            return E()
        case ',': 
            return C()
        case _: 
            return int(ch)





def find_magnitude(input):
    for n in [Number(l) for l in input.splitlines()[:1]]:
        print(n)
        n.reduce()
        print(n)
    

    return 4140

num = Number('[7,[6,[5,[4,[3,2]]]]]')
print(num)
num.reduce()
print(num)

# process_sample_input(find_magnitude)
# process_puzzle_input(find_magnitude)
