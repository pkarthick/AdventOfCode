import strutils

func hasConsecutiveDigits(s: string): bool =
    var count = 0
    for i in 0 .. s.len-2:
        if s[i] == s[i+1]:
            inc count
        else:
            if count == 1:
                return true
            count = 0
    return count == 1

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
            