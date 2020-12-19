import strutils
import sequtils
import tables
import sugar
import algorithm
import sets

let input = """class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9"""

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
let myticket = segs[1].splitLines()[1].split(',').map(x => x.parseInt)
let nearbytickets = segs[2].splitLines()[1..^1].mapIt(it.split(',').map(x => x.parseInt))

var validTickets = newSeq[seq[int]]()

for ticket in nearbytickets:
    var validticket = true

    for val in ticket:
        var valid = false

        for _ , rule in rules:
            if val in rule[0] or val in rule[1]:
                valid = true

        if validticket and not valid:
            validticket = false
    
    if validticket:
        validTickets.add ticket

var invalidColumns = newSeq[(string, int)]()

for ticket in validTickets:
        
    for name, rule in rules:
            
        for column in 0 ..< ticket.len:
            let first = ticket[column] in rule[0]
            let second = ticket[column] in rule[1]

            if not first and not second:
                invalidColumns.add (name, column)

var allRuleSet = (0..19).mapIt(it).toHashSet()

var allRules: seq[(int, string, HashSet[int])]

for name, _ in rules:
    let s = allRuleSet - invalidColumns.filterIt(it[0] == name).mapIt(it[1]).toHashSet
    allRules.add (s.len, name, s)


proc getIndices(): Table[string, int] =
    
    var rules = allRules.sorted()

    result = initTable[string, int]()

    while rules.len > 0:

        case rules[0][2].len
        of 0: 
            rules = rules[1..^1]

        of 1: 
            let ind = rules[0][2].toSeq[0]
            let name = rules[0][1]

            result[name] = ind

            for rule in rules.mitems:
                rule[2].excl(ind)

            rules = rules[1..^1]


        else:
            echo "Unexpected scenario?! What to do now?"
            echo rules
            break


var table = getIndices()

let onlyDepartures = allRules.filterIt(it[1].startsWith("departure"))
echo onlyDepartures.mapIt(table[it[1]]).foldl(a * myticket[b], 1)
