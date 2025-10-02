from collections import defaultdict
from data.day22 import TEST_INPUT, PUZZLE_INPUT

# INPUT = TEST_INPUT
INPUT = PUZZLE_INPUT

# INPUT = """1
# 2
# 3
# 2024"""

secret_numbers = [int(line) for line in INPUT.splitlines()]

def next_secret_number(sn):
    
    sn ^= sn * 64
    sn %= 16777216

    sn ^= sn // 32
    sn %= 16777216

    sn ^= sn * 2048
    sn %= 16777216
    
    return sn

total = 0

bananas = defaultdict(int)

for sn in secret_numbers:
    
    pp = sn % 10
    seq = []
    
    exists = set()

    for _ in range(2000): 
        
        p = sn % 10
        d = p - pp
        seq.append(d) 
        pp = p
        sn = next_secret_number(sn)

        if len(seq) < 4:
            continue

        cseq = tuple(seq[-4:])

        if cseq not in exists:
            exists.add(cseq)
            bananas[cseq] += p

    total += sn

_, max_bananas = max(bananas.items(), key=lambda kv: kv[1])

print(total)
print(max_bananas)

