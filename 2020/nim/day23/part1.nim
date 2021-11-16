import strutils
import sequtils
import sugar

let input="389125467"

var cups = input.map(c => ($c).parseInt)

var cur = 0
var destination = cups[0]
var max = cups.max
let min = cups.min

for i in 0 .. 99:

    var three = newSeq[int]()
    let curcup = cups[cur]
    
    destination = curcup - 1

    if cur < cups.len - 3:
        three = cups[cur + 1 .. cur + 3]
        cups = cups[0 .. cur] & cups[cur+4 .. ^1]
    else:
        let last = cups[cur+1 .. ^1]
        three = cups[cur+1 .. ^1] & cups[0 .. 2-last.len]
        cups = cups[2-last.len+1 .. cur]

    if destination < min: 
        destination = max

    while destination in three:

        if destination <= min: 
            destination = max
        else:
            dec destination

    if destination < min: 
        destination = max

    var ind = (0 ..< cups.len).toSeq.filterIt(cups[it] == destination)[0]
    cups = cups[0 .. ind] & three & cups[ind+1 .. ^1]

    cur = (0 ..< cups.len).toSeq.filterIt(cups[it] == curcup)[0]
    inc cur

    if cur > cups.len - 1: cur = 0


var ind = (0 ..< cups.len).toSeq.filterIt(cups[it] == 1)[0]
inc ind


if ind == cups.len - 1:
    echo (cups[^1 .. ^1] & cups[0 .. ^3]).foldl(a & $b, "")
else:
    echo (cups[ind .. ^1] & cups[0 .. ind-2]).foldl(a & $b, "")