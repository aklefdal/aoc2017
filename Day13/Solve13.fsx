open System.IO

type Input = {
    layer: int
    range: int
}

let lineToInput (s:string) =
    let ss = s.Split([|':'|]) |> Array.map int
    {layer = ss.[0]; range = ss.[1]}

let modFactor (range:int) =
    match range with
    | 1 -> 1
    | _ -> (range - 1) * 2

let isCaught (i:Input) =
    match i.layer % (i.range |> modFactor) with
    | 0 -> true
    | _ -> false

let calculateFactor (i:Input) =
    match i |> isCaught with
    | false -> 0
    | true -> i.layer * i.range

let input = 
    // Path.Combine(__SOURCE_DIRECTORY__, "sample13.txt")
    Path.Combine(__SOURCE_DIRECTORY__, "input13.txt")
    |> File.ReadAllLines
    |> Array.map lineToInput

input |> Array.map calculateFactor |> Array.sum