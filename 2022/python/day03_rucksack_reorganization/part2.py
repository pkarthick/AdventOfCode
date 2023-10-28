import sys

groups = []

lowerOffset = 97 - 1
upperOffset = 65 - 1 - 26


class Group:
    def __init__(self, items, count):
        self.items = items
        self.count = count

    def update(self, items) -> bool:

        if len(self.items.intersection(items)) == 0 or self.count == 3:
            return False

        self.items = self.items.intersection(items)
        self.count += 1
        return True

    def getBadge(self) -> int:
        c = ord(list(self.items)[0])
        return c - (lowerOffset if c >= 97 else upperOffset)


for line in sys.stdin.readlines():
    items = set(line.strip())

    if not any([group.update(items) for group in groups]):
        groups.append(Group(items, 1))


print(sum([group.getBadge() for group in groups]))
