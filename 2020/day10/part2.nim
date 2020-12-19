import strutils
import sequtils
import algorithm
import tables

let input1="""28
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

let input2="""16
10
15
5
1
11
7
19
6
12
4"""



var jolts = input1.splitLines.mapIt(it.parseInt).sorted()
jolts.add jolts[^1]+3

proc countCombinations(jolts: seq[int]): int =
    
    var ct = initCountTable[int]()
    
    let inits = jolts.filterIt(it <= 3)

    for i in 0 ..< len(inits):
        
        if ct.len > 0:
            let len = toSeq(ct.values).foldl(a+b)
            for j in 0 ..< len:
                ct.inc inits[i]

        ct.inc inits[i] 

    for jolt in jolts[inits.len..^1]:

        let keys = toSeq(ct.keys).sorted()

        for k in keys:
            if jolt - k > 3 and ct.hasKey(k):
                ct[k] = 0
            else:
                break

        let len = toSeq(ct.values).foldl(a+b)
        
        ct.inc jolt, len
    
    ct[jolts[^1]]

echo countCombinations(jolts) 
