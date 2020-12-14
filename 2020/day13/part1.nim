import strutils
import sequtils

let input1="""939
7,13,x,x,59,x,31,19"""

let input="""1000509
17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,739,x,29,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,971,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19"""

let lines = input1.splitLines()

let time = lines[0].parseInt
let busIDs = lines[1].split({','}).filterIt(it != "x").mapIt(it.parseInt)

proc getEarliestTime(busID: int): int =
    busID * ((time div busID) + 1)


let (earliest, id) = busIDs.mapIt((getEarliestTime(it), it)).min

let diff = earliest - time

echo diff
echo id

echo diff * id