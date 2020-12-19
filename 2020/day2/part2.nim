import sequtils
import strutils

let input="""1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"""

func checkPassword(info: string): bool =
    
    let 
        segs = info.split({'-', ':', ' '}, maxsplit=4)
        min = segs[0].parseInt
        max = segs[1].parseInt
        ch = segs[2][0]
        pass = segs[4]
        minch = pass[min-1]
        maxch = pass[max-1]
    
    (minch == ch and maxch != ch) or 
        (minch != ch and maxch == ch) 

echo input.splitLines().filterIt(checkPassword(it)).len
