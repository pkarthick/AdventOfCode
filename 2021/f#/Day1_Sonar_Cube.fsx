open System.IO

let day = 1

printfn "Day %i" day

let part1 kind =

  File.ReadAllLines $"../testdata/{day}/{kind}_1.in"
  |> Seq.map int
  |> (fun xs -> Seq.zip xs (Seq.skip 1 xs))
  |> Seq.filter (fun (x, y) -> x < y)
  |> Seq.length

part1 "sample" |> printfn "Part 1 Sample: %A" 
part1 "puzzle" |> printfn "Part 1 Puzzle: %A" 

let part2 kind =

  File.ReadAllLines $"../testdata/{day}/{kind}_2.in"
  |> Seq.map int
  |> (fun xs -> Seq.zip3 xs (Seq.skip 1 xs) (Seq.skip 2 xs))
  |> Seq.map (fun (x, y, z) -> x + y + z)
  |> (fun xs -> Seq.zip xs (Seq.skip 1 xs))
  |> Seq.filter (fun (x, y) -> x < y)
  |> Seq.length

part2 "sample" |> printfn "Part 2 Sample: %A" 
part2 "puzzle" |> printfn "Part 2 Puzzle: %A" 
