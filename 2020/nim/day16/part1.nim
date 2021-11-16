import strutils
import sequtils
import tables
import sugar

let input = """class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"""

proc processRules(lines: seq[string]): Table[string, (Slice[int], Slice[int])] =
    
    result = initTable[string, (Slice[int], Slice[int])]()
    
    for line in lines:
        let segs = line.split(':')
        let ranges = segs[1].split("or")

        let s = ranges.map(proc (r: string): Slice[int] = 
            let xs = r.strip(true, true, {' '}).split('-')
            (xs[0].parseInt .. xs[1].parseInt)
            )

        result[segs[0]] = (s[0], s[1])

let segs = input.split("\n\n")

let rules = processRules(segs[0].splitLines())
let nearbytickets = segs[2].splitLines()[1..^1].mapIt(it.split(',').map(x => x.parseInt))

var scanningerror = 0

for ticket in nearbytickets:
    for val in ticket:
        var valid = false

        for _, rule in rules:
            if val in rule[0] or val in rule[1]:
                valid = true

        if not valid:
            scanningerror += val

echo scanningerror