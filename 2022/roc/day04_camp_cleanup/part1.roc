app "day4_part1"
    packages { pf: "../../../../roc/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout]
    provides [main] to pf

sample =
"""2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""

getRange = \s ->
    s
    |> Str.split "-"
    |> List.map Str.toU32


getSet = \s ->
    when getRange s is
        [Ok l, Ok h] -> List.range l (h+1) |> Set.fromList
        _ -> Set.empty

isProperSubset = \s ->
    when s |> Str.split "," |> List.map getSet is
        [set1, set2] -> 
            len = Set.intersection set1 set2 |> Set.len
            len == Set.len set1 || len == Set.len set2
        _ -> Bool.false

main = 
    sample
    |> Str.split "\n"
    |> List.keepIf isProperSubset
    |> List.len
    |> Num.toStr 
    |> Stdout.line
