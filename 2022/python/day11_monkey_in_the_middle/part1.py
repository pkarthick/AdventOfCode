from dataclasses import dataclass
import sys
from typing import List, Callable


@dataclass
class Monkey:
    items: List[int]
    operation: Callable[[int], int]
    divisor: int
    successIndex: int
    failureIndex: int
    inspectedCount: int = 0

    def inspectItems(self):

        self.inspectedCount += len(self.items)

        while len(self.items) > 0:
            item = self.items.pop(0)
            worryLevel = self.operation(item) // 3
            yield (
                (self.successIndex, worryLevel)
                if worryLevel % self.divisor == 0
                else (self.failureIndex, worryLevel)
            )


class Inspector:
    def __init__(self) -> None:
        lines = [line.strip() for line in sys.stdin.readlines()]
        self.monkeys = []

        for i in range(len(lines) // 7):
            monkeyDetail = lines[i * 7 : (i + 1) * 7]
            # print(monkeyDetail)
            items = list(map(int, monkeyDetail[1].split(": ")[1].split(", ")))

            operation = None
            match monkeyDetail[2].split("= ")[1].split(" "):
                case ["old", "*", "old"]:
                    operation = lambda x: x * x

                case ["old", "*", y]:
                    operation = (lambda y: lambda x: x * y)(int(y))

                case ["old", "+", y]:
                    operation = (lambda y: lambda x: x + y)(int(y))

            divisor = int(monkeyDetail[3].split(" ")[-1])
            successIndex = int(monkeyDetail[4].split(" ")[-1])
            failureIndex = int(monkeyDetail[5].split(" ")[-1])

            self.monkeys.append(
                Monkey(items, operation, divisor, successIndex, failureIndex)
            )

    def inspectMonkeys(self):
        for _ in range(20):
            for monkey in self.monkeys:
                for (ind, worryLevel) in monkey.inspectItems():
                    self.monkeys[ind].items.append(worryLevel)

    def findLevelOfMonkeyBusiness(self):
        self.inspectMonkeys()
        inspection = list(map(lambda m: m.inspectedCount, self.monkeys))
        inspection.sort(reverse=True)
        return inspection[0] * inspection[1]


print(Inspector().findLevelOfMonkeyBusiness())
