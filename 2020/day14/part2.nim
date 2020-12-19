import strutils
import bitops
import algorithm
import tables

var mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
var table = initTable[int, int]()

proc computeMaskCombinations(address: int, val: int, mask: string) =
    var addresses = newSeq[int]()
    addresses.add address
    
    for i, c in mask.reversed():
        case c 
        of 'X': 

            for x in 0 ..< addresses.len:
                addresses[x].clearBit(i)
                
                var ad = addresses[x]
                ad.setBit(i)
                addresses.add ad

        of '1':

            for x in 0 ..< addresses.len:
                addresses[x].setBit(i)

        else: discard

    for addr in addresses:
        table[addr] = val


let input = """mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"""

let lines = input.splitLines()

for line in lines:
    if line.startsWith("mask = "):
        mask = line[7..^1]
    else:
        let segs = line[4..^1].split({']', ' ', '='})
        let address = segs[0].parseInt
        var val = segs[^1].parseInt
        computeMaskCombinations(address, val, mask)

var count = 0

for k, v in table:
    if v != 0:
        count += v

echo count