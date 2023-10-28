app "day5_part1"
    packages { pf: "../../../../roc/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout]
    provides [main] to pf

sample =
"""    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""

sp = sample |> Str.split "\n" |> List.splitFirst ""
stackCount = (l + 1) // 4

stackItem = \stacks, line ->

    List.range 0 (stackCount-1)
    |> List.map \i -> {i, item: List.sublist i*4 ((i+1)*4)}
    |> List.keepIf \{_, item} -> item != ""
    |> List.map \{i, item} -> List.append (List.get stacks i |> Result.withDefault []) item

main = 
    
    

    when sp is
        Ok {before, after} ->  
            l = List.get before 0 |> Result.withDefault "" |> Str.toUtf8 |> List.len 
            stackCount = (l + 1) // 4
            stacks = List.repeat [] stackCount
            stacks |> List.len |> Num.toStr |> Stdout.line 
        _ -> Stdout.line "Not split!"
