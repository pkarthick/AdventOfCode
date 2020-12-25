import strutils
import sugar
import tables

type Node[T] = ref object
  next: Node[T]
  value: T
 
proc newNode[T](v: T): Node[T] =
  Node[T](value: v)
 
proc insertAppend(a, n: var Node) =
  n.next = a.next
  a.next = n

var table = initTable[int, Node[int]]()

let input="389125467"

let max = 1000000
let min = 1

let zero = ($input[0]).parseInt

var cur = newNode zero

table[zero] = cur

var t = cur

for c in input[1..^1]:
    let val = ($c).parseInt
    var v = newNode val
    table[val] = v
    t.insertAppend v
    t = v

for x in 10 .. max:
    var v = newNode x
    table[x] = v
    t.insertAppend v
    t = v

t.next = cur

var i = 0
var triple = [0,0,0]

proc displayList(p: Node[int]) =
    
    echo ""
    
    var s = $p.value

    var t = p
    
    while t.next != nil and t.next != cur:
        s = s & " " & $(t.next.value)
        t = t.next

    if t == nil:
        echo "Not a cycle"

    echo "List: ", s

var next = cur.value

while i < 10000000:

    # echo i

    cur = table[next]

    var destination = cur.value - 1
                    
    if destination < min:
        destination = max

    var t1 = cur.next

    for i in 0..2:
        triple[i] = t1.value
        t1 = t1.next

    next = t1.value

    while true:
        
        if triple.contains(destination):
            dec destination
                    
            if destination < min:
                destination = max
        else:
            break

    if destination < min:
        destination = max

    var afterDestination = table[destination].next
    var curNext = table[triple[2]].next

    table[destination].next = table[triple[0]]

    table[triple[2]].next = afterDestination

    cur.next = curNext

    inc i

var p = table[1]

let n1 = p.next.value
let n2 = p.next.next.value

echo n1
echo n2

echo n1 * n2
