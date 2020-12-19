import strutils
import sequtils

let input="""FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL"""

proc half(min, max: int, id: char, firsthalf: char): (int, int) =
    if id == firsthalf:
        (min, (min+max) div 2)
    else:
        ((min+max+1) div 2, max)

proc process(s: string): int =
    var 
        rmin = 0
        rmax = 127

    for c in s[0..6]:
        (rmin, rmax) = half(rmin, rmax, c, 'F')
        
    var 
        cmin = 0
        cmax = 7
    
    for c in s[7..9]:
        (cmin, cmax) = half(cmin, cmax, c, 'L')
        

    (rmin * 8) + cmin

let seats = input.splitLines.map(process)

echo seats
echo seats.max
