app "day2_part1"
    packages { pf: "../../../../roc/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout]
    provides [main] to pf

sample =
"""A Y
B X
C Z"""

Score: { opponent: U32, mine: U32 }

win = 6
draw = 3
loss = 0

rock = 1
paper = 2
scissors = 3

scores : Dict Str Score
scores = 
    Dict.empty
    |> Dict.insert "A X" { opponent : rock + draw, mine : rock + draw }
    |> Dict.insert "A Y" { opponent: rock + loss, mine: paper + win }
    |> Dict.insert "A Z" { opponent: rock + win, mine: scissors + loss }
    |> Dict.insert "B X" { opponent: paper + win, mine: rock + loss }
    |> Dict.insert "B Y" { opponent: paper + draw, mine: paper + draw }
    |> Dict.insert "B Z" { opponent: paper + loss, mine: scissors + win }
    |> Dict.insert "C X" { opponent: scissors + loss, mine: rock + win }
    |> Dict.insert "C Y" { opponent: scissors + win, mine: paper + loss }
    |> Dict.insert "C Z" { opponent: scissors + draw, mine: scissors + draw }

main = 
    sample
    |> Str.split "\n"
    |> List.walk 0 \score, l -> 
        when scores |> Dict.get l is
            Ok {opponent: _, mine: mine} -> score + mine
            _ -> score
    |> Num.toStr 
    |> Stdout.line
