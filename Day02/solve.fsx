open System.IO
let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let parseLineToInts (line:string) =
    line.Split([|'\t'|]) 
    |> Array.map int

let numbers = 
    File.ReadAllLines inputFile
    |> Array.map parseLineToInts

let maxMinDiff (a: int array) =
    let max = a |> Array.max
    let min = a |> Array.min
    max - min

numbers 
|> Array.sumBy maxMinDiff
