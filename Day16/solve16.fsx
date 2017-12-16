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
Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
|> File.ReadAllText
|> splitLineIntoTokens
|> List.map parseMoves
|> List.fold move start
|> System.String

// Part 2
let oneDance (a:char array) =
    // a b c d e f g h i j k  l  m  n  o  p
    // 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
    // f g m o b e a i j h d  p  k  c  l  n
    [| 
        a.[5]
        a.[6]
        a.[12]
        a.[14]
        a.[1]
        a.[4]
        a.[0]
        a.[8]
        a.[9]
        a.[7]
        a.[3]
        a.[15]
        a.[10]
        a.[2]
        a.[11]
        a.[13]
    |]

let rec iterate i f a =
    match i with
    | 0 -> a
    | _ -> a |> f |> iterate (i - 1) f

start
|> iterate (1000 * 1000 * 1000) oneDance
|> System.String

// ERROR!!! Gives wrong solution :-(
