from dataclasses import dataclass
from itertools import dropwhile, takewhile

input = """Blueprint 1:   Each ore robot costs 4 ore.  Each clay robot costs 2 ore.  Each obsidian robot costs 3 ore and 14 clay.  Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2:  Each ore robot costs 2 ore.  Each clay robot costs 3 ore.  Each obsidian robot costs 3 ore and 8 clay.  Each geode robot costs 3 ore and 12 obsidian."""

lines = input.splitlines()


class Unit:
    def __init__(self, kind) -> None:
        self.kind = kind


obsidian = Unit("obsidian")
clay = Unit("clay")
ore = Unit("ore")


@dataclass
class Cost:
    quantity: int
    kind: str


ORE = "ore"
CLAY = "clay"
OBSIDIAN = "obsidian"
GEODE = "geode"


class BluePrint:
    def __init__(self, line) -> None:
        [first, second] = line.split(":")
        self.blueprint_id = int(first.split(" ")[1])
        requirements = [l.strip().split(" ") for l in second.split(". ")]

        self.minutes = 0

        self.robot_cost = {}

        self.robots = {}
        self.robots[ORE] = 1
        self.robots[CLAY] = 0
        self.robots[OBSIDIAN] = 0
        self.robots[GEODE] = 0

        self.robot_cost[ORE] = [Cost(int(requirements[0][-2]), ORE)]
        self.robot_cost[CLAY] = [Cost(int(requirements[1][-2]), ORE)]
        self.robot_cost[OBSIDIAN] = [
            Cost(int(requirements[2][-5]), ORE),
            Cost(int(requirements[2][-2]), CLAY),
        ]
        self.robot_cost[GEODE] = [
            Cost(int(requirements[3][-5]), ORE),
            Cost(int(requirements[3][-2]), OBSIDIAN),
        ]

        self.raw_materials_quantity = {}
        self.raw_materials_quantity[GEODE] = 0
        self.raw_materials_quantity[OBSIDIAN] = 0
        self.raw_materials_quantity[CLAY] = 0
        self.raw_materials_quantity[ORE] = 0

        self.next_robot_index = 0

    def findQualityLevel(self):

        while self.minutes < 24:

            # robots = {ORE: 0, CLAY: 0, OBSIDIAN: 0, GEODE: 0}

            robotKind = self.nextRobotToBuild()
            self.next_robot_index += 1

            costs = self.robot_cost[robotKind]

            if len(costs) > 1:

                while self.raw_materials_quantity[costs[1].kind] < costs[1].quantity:

                    robots = {ORE: 0, CLAY: 0, OBSIDIAN: 0, GEODE: 0}

                    self.minutes += 1

                    # if self.canBuildRobot(costs[1].kind):
                    for dependentRobotKind in dropwhile(
                        lambda rk: rk != costs[1].kind, [GEODE, OBSIDIAN, CLAY]
                    ):
                        if self.canBuildRobot(dependentRobotKind, robotKind):
                            count = self.buildRobot(dependentRobotKind)
                            robots[dependentRobotKind] += count

                    for material in [ORE, CLAY, OBSIDIAN, GEODE]:
                        self.buildRawMaterial(material)

                    for kind in robots:
                        self.robots[kind] += robots[kind]

                self.minutes += 1

                for material in [ORE, CLAY, OBSIDIAN, GEODE]:
                    self.buildRawMaterial(material)

                self.robots[robotKind] += self.buildRobot(robotKind)

            else:
                minutes = costs[0].quantity

                for _ in range(minutes):
                    for material in [ORE, CLAY]:
                        self.buildRawMaterial(material)
                    self.minutes += 1

                self.minutes += 1

                for material in [ORE, CLAY]:
                    self.buildRawMaterial(material)

                self.robots[material] += self.buildRobot(CLAY)

        return self.raw_materials_quantity[GEODE] * self.blueprint_id

    def canBuildRobot(self, dependentRobotKind, robotKind):

        costs = self.robot_cost[robotKind]

        if dependentRobotKind == GEODE:
            return (
                self.raw_materials_quantity[OBSIDIAN]
                >= self.robot_cost[GEODE][1].quantity
                and self.raw_materials_quantity[ORE]
                >= self.robot_cost[GEODE][0].quantity
            )
        if dependentRobotKind == OBSIDIAN:
            return (
                self.raw_materials_quantity[CLAY]
                >= self.robot_cost[OBSIDIAN][1].quantity
                and self.raw_materials_quantity[ORE]
                >= self.robot_cost[OBSIDIAN][0].quantity
            )
        elif dependentRobotKind == CLAY:
            return (
                costs[1].quantity - self.raw_materials_quantity[costs[1].kind]
                > max(self.robot_cost[CLAY][0].quantity, costs[0].quantity)
                + self.robots[costs[1].kind]
                and self.raw_materials_quantity[costs[1].kind]
                + self.robots[costs[1].kind]
                > costs[1].quantity
            )
            # or (
            #     self.raw_materials_quantity[ORE]
            #     > max(self.robot_cost[CLAY][0].quantity, costs[0].quantity)
            # )

    def nextRobotToBuild(self):

        if self.next_robot_index > 2:
            self.next_robot_index = 2

        return [CLAY, OBSIDIAN, GEODE][self.next_robot_index]

    def findMaxCost(self, robotKind):

        costs = self.robot_cost[robotKind]

        (_, cost) = max([(cost.quantity, cost) for cost in costs])

        return cost

    def costIsSufficient(self, kind):
        return all(
            [
                cost.quantity <= self.raw_materials_quantity[cost.kind]
                for cost in self.robot_cost[kind]
            ]
        )

    def payCostForRobot(self, kind):
        for cost in self.robot_cost[kind]:
            self.raw_materials_quantity[cost.kind] -= cost.quantity

    def buildRobots(self, kind) -> int:

        count = 0

        while self.costIsSufficient(kind):
            self.payCostForRobot(kind)
            count += 1

        return count

    def buildRobot(self, kind) -> int:

        if self.costIsSufficient(kind):
            self.payCostForRobot(kind)
            return 1

        return 0

    def buildRawMaterial(self, kind):

        if self.robots[kind] > 0:
            self.raw_materials_quantity[kind] += self.robots[kind]
        # else:
        #     self.buildRobot(kind)


total = 0

for line in lines:

    blueprint = BluePrint(line)
    qualityLevel = blueprint.findQualityLevel()
    print(qualityLevel, total)
    total += qualityLevel

print(total)
