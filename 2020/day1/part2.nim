import strutils
import sequtils
import algorithm
import options

func pair(elems: seq[int], total: int): Option[(int, int)] =

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
       
        if s < e:
            sum = elems[s] + elems[e]
        else:
            return none((int, int))

    return some((elems[s], elems[e]))

func triple*(elems: seq[int], total: int): (int, int, int) =
    
    for i in 0..<elems.len:
        
        let 
            first = elems[i]
            rem = elems[i+1..^1]
            twin = pair(rem, total - first)

        if twin.isSome:
            let (second, third) = twin.get()
            return (first, second, third)


let input="""1721
979
366
299
675
1456"""

let elems = input.splitLines().map(parseInt).sorted()

let (n1, n2, n3) = triple(elems, 2020)
assert(n1+n2+n3 == 2020)
echo n1 * n2 * n3