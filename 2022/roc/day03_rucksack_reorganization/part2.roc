app "day3_part2"
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

# lowerOffset = 97 - 1
# upperOffset = 65 - 1 - 26

Group: { common: Set U8, count: U8 }

addToGroup : List Group, Set U8 -> List Group
addToGroup = \groups, set ->
    
    if List.isEmpty groups then
       List.append groups {common:set, count:1}
    else
        incomplete = groups |> List.keepIf \{count} -> count < 3
        overlaps = incomplete |> List.map \{common, count} -> 
            overlap = Set.intersection common set
            if overlap == Set.empty then
                {common, count}
            else
                {common:overlap, count: count+1}
        if List.isEmpty overlaps then
            List.append groups {common:set, count:1}
        else
            overlaps
    
main = 
    sample
    |> Str.split "\n"
    |> List.map \s -> s |> Str.toUtf8 |> Set.fromList |> Set.len
    |> List.walk [] addToGroup
    |> List.map \{common} -> common |> Set.toList |> List.first |> Result.withDefault 0
    |> List.sum
    |> Num.toStr 
    |> Stdout.line
