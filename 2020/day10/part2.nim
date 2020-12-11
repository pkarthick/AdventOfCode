import strutils
import sequtils
import algorithm
import sets
import options
import sugar
import deques
import tables

let input="""49
89
70
56
34
14
102
148
143
71
15
107
127
165
135
26
119
46
53
69
134
1
40
81
140
160
33
117
82
55
25
11
128
159
61
105
112
99
93
151
20
108
168
2
109
75
139
170
65
114
21
92
106
162
124
158
38
136
95
161
146
129
154
121
86
118
88
50
48
62
155
28
120
78
60
147
87
27
7
54
39
113
5
74
169
6
43
8
29
18
68
32
19
133
22
94
47
132
59
83
12
13
96
35"""

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



var jolts = input.splitLines.mapIt(it.parseInt).sorted()
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
