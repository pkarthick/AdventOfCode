import strutils
import sequtils
import algorithm
import sugar

let input1="""7,13,x,x,59,x,31,19"""

let input2="""67,7,59,61"""

let input3="""67,x,7,59,61"""

let input4="""67,7,x,59,61"""

let input5="1789,37,47,1889"

let ids = input1.split({','}).mapIt(if it != "x": it.parseInt else: 0)

let pairs = (0 ..< ids.len).mapIt(it).filterIt(ids[it] != 0).mapIt((ids[it], it))

var i = 1

var count = 0

var diff = pairs[0][0]

while i < len(pairs):

    if pairs[1 .. i].allIt((count + it[1]) %% it[0] == 0):
        diff *= pairs[i][0]

        inc i
        
        if i == len(pairs) and (count %% pairs[0][0]) == 0:
            break

        continue
        
    count += diff

echo count
