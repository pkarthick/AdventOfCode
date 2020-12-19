import strutils
import sequtils
import algorithm

func pair*(elems: seq[int], total: int): (int, int) =

    var 
        e = elems.len - 1
        s = 0
        sum = elems[s] + elems[e]

    while sum != total:
        if sum > total:
            dec e
        elif sum < total: #condition improves readability
            inc s
        else:
            break # should not exit here
    
        sum = elems[s] + elems[e]
    
    (elems[s], elems[e])

let input="""1721
979
366
299
675
1456"""

let elems = input.splitLines().map(parseInt).sorted()

let (f, s) = pair(elems, 2020)
assert(f+s == 2020)
echo f * s