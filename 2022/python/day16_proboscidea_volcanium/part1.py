import sys

from typing import List, Tuple
from dataclasses import dataclass

input = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"""

flowRates = {}
connectedValves = {}
valves = []

OPEN = "open"
MOVE = "move"


@dataclass
class Action:
    name: str
    valve: str


class State:
    def __init__(self, valve) -> None:
        self.home = valve
        self.valve = valve
        self.minute = 0
        self.visited = {}
        self.opened = []
        self.pressure = 0
        self.actions = []

    def update(self):
        actions = self.pendingActions()
        # sorted(actions, key=lambda a: flowRates[a.valve])

        states = []

        for action in actions:
            states.append(self.performAction(action))

        return states

    def clone(self):
        state = State(self.home)
        state.valve = self.valve
        state.minute = self.minute
        state.visited = {k: self.visited[k] for k in self.visited}
        state.opened = [x for x in self.opened]
        state.pressure = self.pressure
        state.actions = [Action(x.name, x.valve) for x in self.actions]
        return state

    def performAction(self, action: Action):

        match action:
            case Action("move", valve):
                state = self.clone()
                state.actions.append(action)
                state.move(valve)
                return state

            case Action("open", valve):
                state = self.clone()
                state.actions.append(action)
                state.open()
                return state

    def move(self, valve):
        self.minute += 1
        self.visited[valve] = self.valve
        self.valve = valve

    def open(self):

        opened = set(map(lambda pair: pair[1], self.opened))

        if flowRates[self.valve] > 0 and self.valve not in opened:
            self.minute += 1
            self.opened.append((self.minute, self.valve))
        # self.minute = minute + 1

    def pendingActions(self) -> List[Action]:

        if len(self.opened) == 0 and len(self.visited) == 1:
            return [Action("move", valve) for valve in connectedValves[self.valve]]
        else:

            return [Action("open", self.valve)] + [
                Action("move", valve)
                for valve in connectedValves[self.valve]
                if (valve not in self.visited or (self.visited[valve] != self.valve))
                # and valve != self.home
                and valve not in self.opened
            ]


plural = " tunnels lead to valves "
singular = " tunnels lead to valve "

for line in input.splitlines():
    [first, second] = line.split(";")
    [_, valve, _, _, rate] = first.split(" ")
    [_, rate] = rate.split("=")
    rate = int(rate)

    if second.startswith(plural):
        connected = second[24:].split(", ")
    else:
        connected = [second[23:]]

    valves.append(valve)
    flowRates[valve] = rate
    connectedValves[valve] = connected

startingValve = valves[0]

openableValves = [k for k in flowRates if flowRates[k] > 0]
print(openableValves)


state = State("AA")

pending = {state}

completed = set()

while len(pending) > 0:

    state = pending.pop()

    print("opened:", state.opened)

    if state.minute <= 30:

        if state.opened == openableValves:
            print("All visited!")

        for s in state.update():
            if state not in completed:
                pending.add(s)

    completed.add(state)


# def visitValve(valve, minutesRemaining, action, pressure, opened, visited):

#     # print("Pressure", pressure)
#     # print("Opened", opened)
#     # print("Visited", visited)

#     minutesRemaining -= 1

#     if len(visited) == len(valves):
#         return pressure

#     if minutesRemaining <= 0:
#         return pressure

#     unopenedConnections = [
#         connection for connection in connectedValves[valve] if connection not in opened
#     ]

#     if len(unopenedConnections) == 0:
#         return pressure

#     sorted(unopenedConnections, key=lambda v: flowRates[v], reverse=True)

#     maxPressure = pressure

#     # print("unopenedConnections", unopenedConnections)

#     openedClone = [v for v in opened]

#     for connection in unopenedConnections:

#         print("Minute", 30 - minutesRemaining)

#         if len(opened) == 0:
#             print("No valves are open.")
#         else:
#             total = sum(map(lambda v: flowRates[v], opened))
#             print("Valves", opened, "are open releasing", total, "pressure")

#         if action == "open":
#             print("You open valve", connection)
#             pressure += flowRates[valve]
#         else:
#             print("You move to valve", connection)


#             openedClone.append(connection)

#             visitedClone = {k: visited[k] for k in visited}
#             visitedClone[connection] = valve

#             connectionRate = flowRates[connection] * (minutesRemaining - 1)

#             press = visitValve(
#                 connection,
#                 minutesRemaining,
#                 "open",
#                 pressure + connectionRate,
#                 openedClone,
#                 visitedClone,
#             )

#             if press > maxPressure:
#                 maxPressure = press

#         else:


#             # print("connection", connection)
#             # print("connectionRate", connectionRate)

#             if connection not in visited:

#                 visitedClone = {k: visited[k] for k in visited}
#                 visitedClone[connection] = valve

#                 press = visitValve(
#                     connection,
#                     minutesRemaining,
#                     "move",
#                     pressure,
#                     openedClone,
#                     visitedClone,
#                 )

#                 if press > maxPressure:
#                     maxPressure = press

#     return maxPressure
