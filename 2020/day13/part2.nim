import strutils
import sequtils
import algorithm
import sugar

let input1="""7,13,x,x,59,x,31,19"""

let input="""17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,739,x,29,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,971,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19"""

let input2="""67,7,59,61"""

let input3="""67,x,7,59,61"""

let input4="""67,7,x,59,61"""

let input5="1789,37,47,1889"

let ids = input.split({','}).mapIt(if it != "x": it.parseInt else: 0)

let pairs = (0 ..< ids.len).mapIt(it).filterIt(ids[it] != 0).mapIt((ids[it], it))
echo pairs
echo pairs.len

# let diff = (1 ..< ids.len).mapIt(it).filterIt(ids[it] != 0).mapIt(ids[it]).foldl(a * b, 1)
# echo diff

# var count = diff 
# echo count

var i = 1
# var count = ids[0]
var count = 100005399442412

# var count = 100000000000000 - (100000000000000 %% ids[0]) + ids[0]
# var count = 100005399442412

# echo count %% ids[0]

# echo pairs[1..1].allIt(100003739804341 + it[1] %% it[0] == 0)

# Match found 1
# 100000000000111
# 43
# diff: 748
# Match found 2
# 100000015304939
# 1
# diff: 34
# Match found 3
# 100000020882911
# 0
# diff: 17
# Match found 4
# 100000142203802
# 1
# diff: 34
# Match found 5
# 100005399442412

# echo 100000020882911 - 100000015304939

var diff = pairs[0][0]
# echo count
# echo diff

while i < len(pairs):

    if pairs[1 .. i].allIt((count + it[1]) %% it[0] == 0):
        echo "Match found ", i
        echo count         
        diff *= pairs[i][0]

        inc i
        
        if i == len(pairs) and (count %% pairs[0][0]) == 0:
            break

        else:
            # diff = pairs[i-2][0] * pairs[i-1][0]

            echo "diff: ", diff

        continue
        
    count += diff
    # echo count

