open System.IO

let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let parseLine (line:string) =
    line.Split([|' '|]) 
    |> Array.toList

let words = 
    File.ReadAllLines inputFile
    |> Array.toList
    |> List.map parseLine

let rec isValid (l:string list) =
    match l with
    | [] -> true
    | h::t ->
        match t |> List.contains h with
        | true -> false
        | _ -> isValid t

words
|> List.filter isValid
|> List.length

// Part 2
let sortWord (s:string) =
    s.ToCharArray() |> Array.sort |>  (fun s -> System.String s)

words
|> List.map (List.map sortWord)
|> List.filter isValid
|> List.length
