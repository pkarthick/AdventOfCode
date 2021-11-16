import strutils
import sequtils
import algorithm

let input="""28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""

let jolts = input.splitLines.mapIt(it.parseInt).sorted()

var prev = 0
var diffs = newSeq[int]()

for jolt in jolts:
    let diff = jolt - prev    
    diffs.add diff

    prev = jolt

    if diff < 1 and diff > 3:
        echo "wrong difference!!!!!!!"
        break

# echo jolts
# echo diffs

let ones = diffs.filterIt(it == 1).len()
let threes = diffs.filterIt(it == 3).len() + 1

echo ones * threes
