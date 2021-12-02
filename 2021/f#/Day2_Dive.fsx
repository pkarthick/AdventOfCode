open System.IO

let day = 2

printfn "Day %i" day

let (|Forward|Up|Down|) dir = 
  match dir with 
  | "forward" -> Forward
  | "up" -> Up
  | "down" -> Down
  | _ -> raise (System.ArgumentException("Unexpected Instruction Format!"))

let part1 kind =

  let processInstruction (d, w) (dir, dis) = 
    match dir with
    | Forward -> (d, w+dis)
    | Up -> (d-dis, w)
    | Down -> (d+dis, w)

  File.ReadAllLines $"../testdata/{day}/{kind}_1.in"
  |> Seq.map (fun l -> 
                let ws = l.Split(' ')
                (ws[0], int(ws[1])))
  |> Seq.fold processInstruction (0,0) 
  |> fun (d, w) -> d * w

part1 "sample" |> printfn "Part 1 Sample: %A" 
part1 "puzzle" |> printfn "Part 1 Puzzle: %A" 

let part2 kind =

  let processInstruction (d, w, a) (dir, dis) = 
    match dir with
    | Forward -> (d+ a * dis, w+dis, a)
    | Up -> (d, w, a-dis)
    | Down -> (d, w, a+dis)

  File.ReadAllLines $"../testdata/{day}/{kind}_2.in"
  |> Seq.map (fun l -> 
                let ws = l.Split(' ')
                (ws[0], int(ws[1])))
  |> Seq.fold processInstruction (0,0,0) 
  |> fun (d, w, _) -> d * w

part2 "sample" |> printfn "Part 2 Sample: %A" 
part2 "puzzle" |> printfn "Part 2 Puzzle: %A" 

