from collections import defaultdict
from data.day05 import PUZZLE_INPUT

top, bottom = PUZZLE_INPUT.split('\n\n')

befores = defaultdict(set)

for before, after in map(lambda s: map(int, s.split('|')), top.splitlines()):
    befores[after].add(before)

sequences = list(map(lambda s: list(map(int, s.split(','))), bottom.splitlines()))

def is_before(bef, aft):
    if aft in befores: 
        if bef in befores[aft]:
            return True
        
        if aft in befores[bef]:
            return False
        
        return any(map(lambda b: is_before(bef, b), befores[aft]))
    else:
        if aft in befores[bef]:
            return False

total = 0

for sequence in sequences:
    for bi in range(len(sequence)):
        if any(map(lambda b: not is_before(b, sequence[bi]), sequence[:bi])):
            break
    else:
        total += sequence[len(sequence)//2]

print(total)

si = 0
incorrects = set()

def in_order(b):
    if not is_before(b, sequence[bi]):
        sequence[bi-1], sequence[bi] = sequence[bi], sequence[bi-1]
        incorrects.add(si)
        return False
    
    return True


while si < len(sequences):

    sequence = sequences[si]

    for bi in range(len(sequence)):
        if not all(map(in_order, sequence[:bi])):
            break
    else:
        si += 1
   

print(sum([seq[len(seq)//2] for si in incorrects if (seq := sequences[si]) ]))
    
