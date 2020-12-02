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

