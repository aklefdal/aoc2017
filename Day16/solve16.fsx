open System.IO
let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

type Moves =
| Spin of int
| Exchange of int * int
| Partner of char * char

let parseSpin (s:string) =
    s.Substring(1)
    |> int
    |> Spin

let parseExchange (s:string) =
    let chars = s.ToCharArray()
    Exchange (chars.[1], chars.[3])

let parsePartner (s:string) =
    let chars = s.ToCharArray()
    Partner (chars.[1], chars.[3])

let parseMove (s:string) =
    match s with
    | s when s.StartsWith("s") -> s |> parseSpin
    | s when s.StartsWith("x") -> s |> parseExchange
    | s when s.StartsWith("p") -> s |> parsePartner
    | _ -> failwithf "Unknown move %s" s

let parseLineToMoves (line:string) =
    line.Split([|','|]) 
    |> Array.toList

let numbers = 
    File.ReadAllText inputFile
    |> parseLineToMoves
    |> List.map parseMove



"x3/6" |> parseMove
"pp/k" |> parsePartner
"x10/1" |> parseMove

let s = "pp/k"
let c = [|'/'|]
let ints = s.Substring(1).Split(c)
Partner (int ints.[0], int ints.[1])
