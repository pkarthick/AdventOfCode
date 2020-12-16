import strutils
import sequtils

let input="15,5,1,4,7,0"
let input1="0,3,6"

let nums = input.split({','}).mapIt(it.parseInt)

var pairs: array[0 .. 30000000 - 1, (int, int)]
let turn = 30000000

for i, num in nums:
    pairs[num] = (0, i+1)
    if num == 0:
        pairs[0] = (i+1, nums.len+1)

if pairs[0] == (0,0): # 0 may be unspoken, first spoken after the fixed entries 
    pairs[0] = (0, nums.len+1)

proc findNumber(): int =

    var next = 0
    var i = len(nums) + 2
            
    while true:
        if pairs[next] == (0,0):
                
            pairs[next] = (0, i)
            next = 0
            inc i
        else:
            let (prev1, prev) = pairs[next]

            if prev1 == 0:
                pairs[next] = (prev, i)
                next = 0
                inc i
            else:            
                next = prev - prev1
                
                if pairs[next] == (0,0): 
                    
                    pairs[next] = (0, i)
                    inc i
                    if i > turn: break
                    next = 0
                                    
                var (_, p) = pairs[next]
                pairs[next] = (p, i)
                inc i

        if i > turn: break

    return next

echo findNumber()