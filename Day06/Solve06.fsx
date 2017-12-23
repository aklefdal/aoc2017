open System.IO
open System

let splitOnTabs (s:string) = s.Split([|'\t'|]) 

let input = 
    Path.Combine(__SOURCE_DIRECTORY__, "input06.txt")
    |> File.ReadAllText
    |> splitOnTabs
    |> Array.map int

// let input = [|0;2;7;0|]

let hex (i:int) = 
    match i with
    | n when n >= 16 -> failwith "Oops. Too big."
    | n when n < 10 -> char (n + 0x30) 
    | n -> char (n + 0x37)

let calculateHash (a:int array) = 
    a |> Array.map hex |> String

type ShareState = {
    pos: int
    rest: int
}

let arrayLength = input.Length

let rec giveToNext (a:int array) shareState =
    let { pos = pos;rest = rest } = shareState 
    match pos >= arrayLength with
    | false -> a.[pos] <- a.[pos] + 1; { pos = (pos + 1); rest = (rest - 1) }
    | true -> giveToNext a { pos = 0; rest = rest }

let shareHighest (a:int array) = 
    let max = a |> Array.max
    let maxPos = a |> Array.findIndex (fun i -> i = max)
    let rest = a.[maxPos]
    let newArray = a |> Array.copy
    newArray.[maxPos] <- 0

    let rec share shareState =
        match shareState.rest > 0 with
        | false -> ()
        | true -> 
            giveToNext newArray shareState |> share
    share {pos = maxPos + 1; rest = rest}
    newArray

let rec findFirstDuplicate (l:string list) (a:int array) =
    let nextA = a |> shareHighest
    let hash = calculateHash nextA
    match l |> List.contains hash with
    | true -> 
        let pos = l |> List.findIndex (fun s -> s = hash)
        ((l |> List.length) + 1, pos + 1)
    | false -> findFirstDuplicate (hash::l) nextA

input |> findFirstDuplicate []