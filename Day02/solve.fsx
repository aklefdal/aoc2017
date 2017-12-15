open System.IO
let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let parseLineToInts (line:string) =
    line.Split([|'\t'|]) 
    |> Array.toList
    |> List.map int

let numbers = 
    File.ReadAllLines inputFile
    |> Array.toList
    |> List.map parseLineToInts

let maxMinDiff (a: int list) =
    let max = a |> List.max
    let min = a |> List.min
    max - min

numbers 
|> List.sumBy maxMinDiff

// Part 2
let rec findNumbersDivisibleWith e l = // assume sorted list, with largest first
    match l with
    | [] -> None
    | h::t -> 
        match e % h with
        | 0 -> Some (e, h)
        | _ -> findNumbersDivisibleWith e t

let rec findDivisibleNumbers l =
    match l with
    | [] -> None
    | h::t ->
        match findNumbersDivisibleWith h t with
        | Some (a, b) -> Some (a / b)
        | None -> findDivisibleNumbers t 

numbers
|> List.map (List.sortDescending >> findDivisibleNumbers)
|> List.sumBy Option.get
