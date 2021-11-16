import sequtils
import strutils
import tables

type 

    Bag = object
        color: string
        inner: seq[tuple[quantity: int, color: string]]
        

let input ="""light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""

proc newBag(line: string): Bag =

    let 
        segs = line.split(" bags contain ")
        innerlist = segs[1].split({',', '.'})
        
    var    
        inner = newSeq[tuple[quantity: int, color: string]]()
    
    for l in innerlist:
        let bagsegs = l.split(" ").filterIt(it.len > 0)
        
        if bagsegs.len > 0 and bagsegs[0] != "no":
            let 
                quantity = bagsegs[0].parseInt
                color = bagsegs[1] & " " & bagsegs[2]

            inner.add (quantity, color)

    Bag(color: segs[0], inner: inner)
    
let bags = input.split('\n').filterIt(it.len > 0).mapIt(newBag(it))
let bagsTable = bags.mapIt((it.color, it)).toSeq.toTable
let colorToFind = "shiny gold"

proc countBags(color: string): int =
    return bagsTable[color].inner.foldl(a + b.quantity + (b.quantity * countBags(b.color)), 0)
      
echo countBags(colorToFind)