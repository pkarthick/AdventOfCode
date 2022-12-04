app "day3_part1"
    packages { pf: "../../../../roc/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout]
    provides [main] to pf

sample =
"""vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""

lowerOffset = 97 - 1
upperOffset = 65 - 1 - 26

getPriority : Str -> U32
getPriority = \s ->
    chars = Str.toUtf8 s
    half = List.len chars // 2
    set1 = Set.fromList (List.takeFirst chars half)
    set2 = Set.fromList (List.takeLast chars half)
    common = Set.intersection set1 set2 |> Set.toList
    
    priority = when common is
        [ch] if ch >= 97 -> ch - lowerOffset
        [ch] -> ch - upperOffset
        _ -> 0

    Num.toU32 priority
    
main = 
    sample
    |> Str.split "\n"
    |> List.walk 0 \priority, s -> Num.toU32 (getPriority s) + priority
    |> Num.toStr 
    |> Stdout.line
