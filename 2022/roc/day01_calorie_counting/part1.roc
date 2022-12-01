app "day1_part1"
    packages { pf: "../../../../roc/examples/cli/cli-platform/main.roc" }
    imports [pf.Stdout]
    provides [main] to pf

sample = 
"""1000
2000
3000

4000

5000
6000

7000
8000
9000

10000""" 

parse : Str -> List U32
parse = \i ->
    i 
    |> Str.split "\n\n"
    |> List.map  \s ->
        s 
        |> Str.split "\n"
        |> List.keepOks Str.toU32
        |> List.walk 0 Num.add

main = 
    sample
    |> parse
    |> List.max 
    |> Result.withDefault 0 
    |> Num.toStr 
    |> Stdout.line
