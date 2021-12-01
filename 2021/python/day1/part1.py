day = 1
part = 1

def getInput(kind):
  with open(f'../testdata/{day}/{kind}_{part}.in') as fo:
    return fo.read()

def getOutput(kind):
  with open(f'../testdata/{day}/{kind}_{part}.out') as fo:
    return fo.read()

kind = "sample"

input = getInput(kind)
expected = int(getOutput(kind))

lines = input.splitlines()

def get_increase_count():
  return len(list(filter(lambda x: int(x[0]) < int(x[1]), zip(lines, lines[1:]))))

actual = get_increase_count()

print(f'{kind} Output:')
print(actual)

assert(actual == expected)

kind = "puzzle"

input = getInput(kind)
expected = int(getOutput(kind))
lines = input.splitlines()
actual = get_increase_count()

print(f'{kind} Output:')
print(actual)

assert(actual == expected)
  
