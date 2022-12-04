import sys

count = 0

for line in sys.stdin.readlines():
    [f, s] = line.strip().split(",")
    [f1, f2] = list(map(int, f.split("-")))
    [s1, s2] = list(map(int, s.split("-")))

    set1 = set(range(f1, f2 + 1))
    set2 = set(range(s1, s2 + 1))

    if set1.issuperset(set2) or set2.issuperset(set1):
        count += 1

print(count)
