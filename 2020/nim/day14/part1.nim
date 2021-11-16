import strutils
import bitops
import algorithm
import tables

let input = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"""

var mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
var table = initTable[int, int]()

proc computeBits (address: int, val: var int, mask: string) =
    
    let x = val.toBin(36).reversed()
    
    discard table.hasKeyOrPut(address, 0)

    for i, m in mask.reversed():
        case m
        of 'X': 
            if x[i] == '1': 
                val.setBit(i) 
            else: 
                val.clearBit(i)
        of '1': val.setBit(i)
        of '0': val.clearBit(i)
        else: discard

    table[address] = val
    
let lines = input.splitLines()

for line in lines:
    if line.startsWith("mask = "):
        mask = line[7..^1]
    else:
        let segs = line[4..^1].split({']', ' ', '='})
        let address = segs[0].parseInt
        var val = segs[^1].parseInt
        
        computeBits(address, val, mask)

var count = 0

for k, v in table:
    if v != 0:
        count += v

echo count


