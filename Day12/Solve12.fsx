open System.IO

type Input = {
    village: int
    pipes: int list
}

let lineToInput (s:string) =
    let village = s.Split([|' '|]).[0] |> int
    let pipes = s.Split([|'>'|]).[1].Trim().Split([|','|]) |> Array.toList |> List.map int
    {village = village; pipes = pipes}

let input = 
    //Path.Combine(__SOURCE_DIRECTORY__, "sample12.txt")
    Path.Combine(__SOURCE_DIRECTORY__, "input12.txt")
    |> File.ReadAllLines
    |> Array.map lineToInput

let rec add (l:int list) (n:int) =
    match l |> List.contains n with
    | true -> l
    | false -> 
        input.[n].pipes |> List.fold add (n::l)

add [] 0 |> List.sort |> List.length