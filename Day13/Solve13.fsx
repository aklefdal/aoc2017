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

let isCaught (time:int) (range:int) =
    match time % (range |> modFactor) with
    | 0 -> true
    | _ -> false

let calculateFactor (delay:int) (i:Input) =
    match isCaught (i.layer + delay) i.range with
    | false -> 0
    | true -> i.layer * i.range

let input = 
    // Path.Combine(__SOURCE_DIRECTORY__, "sample13.txt")
    Path.Combine(__SOURCE_DIRECTORY__, "input13.txt")
    |> File.ReadAllLines
    |> Array.map lineToInput

let calculateSeverity delay =
    Array.sumBy (calculateFactor delay)


// Part 1
input |> calculateSeverity 0

// Part 2
let isCaughtAtAll delay input =
    input
    |> Array.map (fun i -> isCaught (i.layer + delay) i.range)
    |> Array.contains true 

seq {0..10000000} |> Seq.find (fun delay -> input |> isCaughtAtAll delay |> (=) false)