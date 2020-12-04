import strutils
import sequtils
import sets
import algorithm

func hasConsecutiveDigits(s: string): bool =
    for i in 0 .. s.len-2:
        if s[i] == s[i+1]:
            return true
    return false

func isAscending(s: string): bool =
    for i in 0 .. s.len-2:
        if s[i] > s[i+1]:
            return false
    return true

func countProximity(min, max: int): int =
    result = 0
    for i in min .. max:
        let s = i.intToStr
        if s.hasConsecutiveDigits and s.isAscending:
            inc result

echo countProximity(372037, 905157)
            