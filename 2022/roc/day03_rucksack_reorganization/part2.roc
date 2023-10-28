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

updateGroup: Group, Set U8 -> Result Group Group
updateGroup = \{common, count}, set ->
    when count is
        3 -> Err {common, count}
        _ -> 
            common1 = Set.intersection common set
            if Set.intersection common set == Set.empty then
                Err {common, count}
            else
                Ok {common: common1, count: count+1}

addToGroup : List Group, Set U8 -> List Group
addToGroup = \groups, set ->
    
    if List.isEmpty groups then
       List.append groups {common:set, count:1}
    else
        
        newGroups = List.map groups \group -> updateGroup group set
                
        if List.any newGroups List.isOk then
            newGroups
        else
            List.append groups {common:set, count:1}
    
main = 
    sample
    |> Str.split "\n"
    |> List.map \s -> s |> Str.toUtf8 |> Set.fromList |> Set.len
    |> List.walk [] addToGroup
    |> List.map \{common} -> common |> Set.toList |> List.first |> Result.withDefault 0
    |> List.sum
    |> Num.toStr 
    |> Stdout.line
