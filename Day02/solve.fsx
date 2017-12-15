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


// Part 1

let maxMinDiff (a: int list) =
    (List.max a) - (List.min a)

numbers 
|> List.sumBy maxMinDiff

// Part 2
let rec findNumbersDivisibleWith e l = // assume sorted list, with largest first
    match l with
    | [] -> None
    | h::t -> 
        match e % h with
        | 0 -> Some (e / h)
        | _ -> findNumbersDivisibleWith e t

let rec findDivisibleNumbers l =
    match l with
    | [] -> 0
    | h::t ->
        match findNumbersDivisibleWith h t with
        | Some i -> i
        | None -> findDivisibleNumbers t 

numbers 
|> List.sumBy (List.sortDescending >> findDivisibleNumbers)
