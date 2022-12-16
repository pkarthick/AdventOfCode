import sys


def diff(x1, x2):
    if x1 < 0:
        if x2 < 0:
            return abs(-x1 - -x2)
        else:
            return x2 + abs(x1)
    else:
        if x2 < 0:
            return x1 + abs(x2)
        else:
            return abs(x1 - x2)


minx = 100000000000
maxx = 0

beaconsUnavailable = set([])
beaconsAvailable = set([])

targety = 2000000

for line in sys.stdin.readlines():
    line = line.strip()
    tokens = line.split()
    sensorx = int(tokens[2].rstrip(",").split("=")[1])
    sensory = int(tokens[3].rstrip(":").split("=")[1])

    beacony = int(tokens[-1].split("=")[1])
    becaonx = int(tokens[-2].rstrip(",").split("=")[1])

    if beacony == targety:
        beaconsAvailable.add(becaonx)

    if sensorx > maxx:
        maxx = sensorx

    if becaonx > maxx:
        maxx = becaonx

    if sensorx < minx:
        minx = sensorx

    if becaonx < minx:
        minx = becaonx

    sensorstrength = diff(sensorx, becaonx) + diff(sensory, beacony)

    yd = abs(sensory - targety)

    if yd <= sensorstrength:
        xr = sensorstrength - yd

        for xd in range(xr + 1):
            beaconsUnavailable.add(sensorx - xd)
            beaconsUnavailable.add(sensorx + xd)
count = len(beaconsUnavailable) - len(beaconsAvailable)

print(count)
