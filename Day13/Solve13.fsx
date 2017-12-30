open System.IO

type Input = {
    layer: int
    range: int
}

let lineToInput (s:string) =
    let ss = s.Split([|':'|]) |> Array.map int
    {layer = ss.[0]; range = ss.[1]}

let input = 
    Path.Combine(__SOURCE_DIRECTORY__, "sample13.txt")
    //Path.Combine(__SOURCE_DIRECTORY__, "input13.txt")
    |> File.ReadAllLines
    |> Array.map lineToInput
