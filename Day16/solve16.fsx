open System.IO

type Moves =
| Spin of int
| Exchange of int * int
| Partner of char * char

let parseSpin (s:string) =
    s.Substring(1)
    |> int
    |> Spin

let parseExchange (s:string) =
    let c = [|'/'|]
    let ints = s.Substring(1).Split(c)
    Exchange (int ints.[0], int ints.[1])

let parsePartner (s:string) =
    let chars = s.ToCharArray()
    Partner (chars.[1], chars.[3])

let parseMoves (s:string) =
    match s with
    | s when s.StartsWith("s") -> s |> parseSpin
    | s when s.StartsWith("x") -> s |> parseExchange
    | s when s.StartsWith("p") -> s |> parsePartner
    | _ -> failwithf "Unknown move %s" s

let spin (a:char array) (i:int) =
    let countBefore = a.Length - i
    let aBefore = a |> Array.take countBefore
    let aAfter = a |> Array.skip countBefore
    Array.concat [aAfter;aBefore]

let exchange (a:char array) ((i:int), (j:int)) =
    let b = Array.create 16 ' '
    b |> Array.iteri (fun k _ ->
                        match k with
                        | x when x = i -> b.[k] <- a.[j]
                        | x when x = j -> b.[k] <- a.[i]
                        | _ -> b.[k] <- a.[k])
    b

let partner (a:char array) ((ci:char), (cj:char)) =
    let i = a |> Array.findIndex (fun e -> e = ci)
    let j = a |> Array.findIndex (fun e -> e = cj)
    exchange a (i, j)

let move (a:char array) m =
    match m with
    | Spin i -> spin a i
    | Partner (b, c) -> partner a (b, c)
    | Exchange (i, j) -> exchange a (i, j)
   
let splitLineIntoTokens (line:string) =
    line.Split([|','|]) 
    |> Array.toList

let start = seq {'a' .. 'p'} |> Seq.toArray

let moves =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText
    |> splitLineIntoTokens
    |> List.map parseMoves

let doWork (positions:char array) _ =
    moves |> List.fold move positions

doWork start 0
|> System.String
|> printfn "Solution to part 1: %s"


// Part 2
let rec findModus (positions:char array) (count:int) =
    let newPositions = doWork positions 0 
    match newPositions = start with
    | true -> count
    | false -> findModus newPositions (count + 1)

let modus = findModus start 1
let count = 1000*1000*1000 % modus
seq {1..count} 
|> Seq.fold doWork start 
|> System.String
|> printfn "Solution to part 2: %s"
