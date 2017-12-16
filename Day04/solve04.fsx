open System.IO
let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let parseLine (line:string) =
    line.Split([|' '|]) 
    |> Array.toList

let words = 
    File.ReadAllLines inputFile
    |> Array.toList
    |> List.map parseLine

let a = "aa bb cc dd ee"
let b = "aa bb cc dd aa"
let c = "aa bb cc dd aaa"

