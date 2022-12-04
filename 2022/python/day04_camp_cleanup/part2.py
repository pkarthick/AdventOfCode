import sys

count = 0

for line in sys.stdin.readlines():
    [f, s] = line.strip().split(",")
    [f1, f2] = list(map(int, f.split("-")))
    [s1, s2] = list(map(int, s.split("-")))

    set1 = set(range(f1, f2 + 1))
    set2 = set(range(s1, s2 + 1))

    if not set1.isdisjoint(set2):
        count += 1

print(count)
