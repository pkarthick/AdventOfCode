from data.day05 import PUZZLE_INPUT

top, bottom = PUZZLE_INPUT.split('\n\n')

befores = {}

for [before, after] in map(lambda s: map(int, s.split('|')), top.splitlines()):
    if after in befores:
        befores[after].add(before)
    else:
        befores[after] = set([before])

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

while si < len(sequences):

    sequence = sequences[si]

    in_order = True

    for bi in range(len(sequence)):
        bs = sequence[:bi]
    
        for b in bs:
            if not is_before(b, sequence[bi]):
                v = sequence[bi-1]
                sequence[bi-1] = sequence[bi]
                sequence[bi] = v
                in_order = False
                incorrects.add(si)
                break

        if not in_order:
            break
    
    if in_order:
        si += 1
   

print(sum([sequences[si][len(sequences[si])//2] for si in incorrects]))
    
