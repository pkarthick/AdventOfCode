import strutils
import sequtils

let input1 = "1 + 2 * 3 + 4 * 5 + 6"
let input2 = "1 + (2 * 3) + (4 * (5 + 6))"
let input3 = "2 * 3 + (4 * 5)"
let input4 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"
let input5 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
let input6 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

type

    Token = ref object of RootObj

    Operator = ref object of Token
        op: char

    Number = ref object of Token
        val: int64

    Open = ref object of Token

    Close = ref object of Token

method `$`(t: Token): string {.base.} =
    quit "overide token tostring!"

method `$`(t: Number): string =
    $t.val

method `$`(t: Operator): string =
    $t.op

method `$`(t: Open): string =
    "("

method `$`(t: Close): string =
    ")"

proc parseExpression(exp: string): seq[Token] =

    result = newSeq[Token]()
    var digits = ""

    for c in exp:
        if c in '0' .. '9':
            digits = digits & $c
        elif c in @['+', '-', '*', '(', ')']:

            if digits != "":
                result.add Number(val: digits.parseInt)
                digits = ""

            case c
            of '+', '-', '*':
                result.add Operator(op: c)
            of '(':
                result.add Open()
            of ')':
                result.add Close()
            else:
                discard
        else:
            discard

    if digits != "":
       result.add Number(val: digits.parseInt)

method computeExpression(t1: Token, t2: Token, t3: Token): Number {.base.} =
    echo t1
    echo t2
    echo t3

    quit "override base. in compute expression!"

method computeExpression(op1: Number, op2: Number, op: Operator): Number =
    
    case op.op
    of '+': result = Number(val: op1.val + op2.val)
    of '*': result = Number(val: op1.val * op2.val)
    of '-': result = Number(val: op1.val - op2.val)
    else: discard


proc createExpression(tokens: seq[Token]): int64 = 

    proc computeBrackets(i: var int): int64 =
        var 
            open = 1
            stack = newSeq[seq[Token]]()
            last = newSeq[Token]()
        
        result = 0'i64

        while i < tokens.len:

            if tokens[i] of Open:
                open += 1
                
                stack.add last
                last = newSeq[Token]()
                inc i
                continue

            elif tokens[i] of Close:
                open -= 1
                result = createExpression last
                if open == 0:
                    inc i
                    break
                else:
                    
                    last = stack[^1]
                    last.add Number(val: result)
                    result = createExpression last
                    inc i
                    continue


            last.add tokens[i]
            inc i        

    if tokens.len >= 3:
        let t0 = tokens[0] 
        let t2 = tokens[2]
        let t1 = tokens[1]

        if t0 of Number and t1 of Close: 
            let tokens1 = concat(@[tokens[0]], tokens[2 .. ^1])
            return createExpression tokens1

        elif t0 of Open:

            var i = 1
            let r = computeBrackets(i)
            let t: Token = Number(val: r)
            let tokens1 = concat(@[t], tokens[i .. ^1])
                
            return createExpression tokens1

        elif t1 of Operator:
            if t0 of Number and t2 of Number:

                let t: Token = computeExpression(t0, t2, t1)
                
                let tokens1 = concat(@[t], tokens[3 .. ^1])
                
                return createExpression tokens1

            elif t0 of Number and t2 of Open:
                var i = 3
                let r = computeBrackets(i)
                let tokens1 = concat(@[t0, t1, Number(val: r)], tokens[i .. ^1])
                return createExpression tokens1

            else:
                quit "Unexpected Scenario 1!"

        else:
            echo tokens
            quit "Unexpected Scenario 2!"

    elif tokens.len == 1 and tokens[0] of Number:
        return Number(tokens[0]).val
        
var sum = 0'i64

for line in input6.splitLines():
    let tokens = parseExpression(line)
    let s = createExpression tokens
    sum += s

echo sum