open System

let rec getSection (ring:int array) (current:int) (sectionLength:int) (section:int list) = 
    match sectionLength with
    | 0 -> section
    | _ -> 
        match current with
        | c when c >= ring.Length -> 
            getSection ring (1) (sectionLength - 1) (ring.[0]::section)
        | _ -> 
            getSection ring (current + 1) (sectionLength - 1) (ring.[current]::section)

let rec setSection (ring:int array) (current:int) (section:int list) = 
    match section with
    | [] -> ()
    | h::t -> 
        match current with
        | c when c >= ring.Length ->
            ring.[0] <- h
            setSection ring 1 t
        | _ -> 
            ring.[current] <- h
            setSection ring (current + 1) t

let reverseSection (ring:int array) (current:int) (skipLength:int) (sectionLength:int) =
    getSection ring current sectionLength [] |> setSection ring current
    let newPossibleCurrent = current + sectionLength + skipLength
    if newPossibleCurrent >= ring.Length then
        (newPossibleCurrent % ring.Length, skipLength + 1)
    else
        (newPossibleCurrent, skipLength + 1)    

let rec doWork (ring:int array) (current:int) (skipLength:int) (rest:int list) =
    match rest with
    | [] -> (current, skipLength)
    | h::t ->
        let (newCurrent, newSkipLength) =  reverseSection ring current skipLength h
        doWork ring newCurrent newSkipLength t

let rec doWork2 (current:int) (skipLength:int) (input:int list) (counter:int) (ring:int array) =
    match counter with
    | 0 -> ring 
    | _ ->
        let (newCurrent, newSkipLength) = doWork ring current skipLength input
        doWork2 newCurrent newSkipLength input (counter - 1) ring

let rec createDenseHashNumbers (result:int list) (sparseHash:int list) =
    match sparseHash with
    | [] -> result
    | _ ->
        let denseHash = sparseHash |> List.take 16 |> List.reduce (^^^)
        createDenseHashNumbers (denseHash::result) (sparseHash |> List.skip 16) 
let rec createDenseHash (sparseHash:int list) =
    sparseHash |> createDenseHashNumbers [] |> List.rev

let hex (i:int) = 
    match i with
    | n when n >= 16 -> failwith "Oops. Too big."
    | n when n < 10 -> char (n + 0x30) 
    | n -> char (n + 0x57)

let createHex (i:int) =
    [|
        i / 16 |> hex
        i % 16 |> hex
    |]
    |> String

let hash (s:string) =
    let input = s |> Seq.toList |> List.map (byte >> int)
    let defaultSectionEnding = [17; 31; 73; 47; 23]
    let section = input @ defaultSectionEnding
    
    seq {0..255} 
    |> Seq.toArray
    |> doWork2 0 0 section 64 
    |> Array.toList 
    |> createDenseHash
    // |> List.map createHex
    // |> List.reduce (+)

// Part 1
let rec countBinaryOnes (sum:int) (i:int) =
    match i with
    | 0 -> sum
    | 1 -> sum + 1
    | _ -> 
        match i % 2 with
        | 0 -> countBinaryOnes sum (i / 2) 
        | 1 -> countBinaryOnes (sum + 1) (i / 2) 

let countUsedCellsForRow (l:int list) =
    l |> List.sumBy (countBinaryOnes 0)

let input = "xlqgujun"

seq {0..127} 
|> Seq.map (fun rowNumber -> sprintf "%s-%i" input rowNumber |> hash)
|> Seq.sumBy countUsedCellsForRow

// Part 2
let rec addBinaryDigit (l:bool list) (i:int) =
    match i with
    | 0 -> false::l
    | 1 -> true::l
    | _ -> 
        match i % 2 with
        | 0 -> addBinaryDigit (false::l) (i / 2) 
        | 1 -> addBinaryDigit (true::l) (i / 2) 

let rec toLongList (l:bool list) =
    match l.Length with
    | 8 -> l
    | _ -> false::l |> toLongList

let createBinaryListForRow =
    List.map (addBinaryDigit [])
    >> List.map toLongList
    >> List.reduce (@)

type State =
| Unprocessed
| Processed of int
| NotSet

let toState b =
    match b with
    | true -> Unprocessed
    | false -> NotSet

let stateToNumber state =
    match state with
    | Unprocessed -> 0
    | Processed i -> i
    | NotSet -> 0


let getRow rowNumber =
    sprintf "%s-%i" input rowNumber |> hash
    |> createBinaryListForRow
    |> List.map toState
    |> List.toArray

let disk = 
    seq {0..127} 
    |> Seq.map getRow
    |> Seq.toArray

let rec processCell (currentRow:int) (currentCell:int) (currentGroup:int) =
    match currentRow with
    | r when r >= 128 -> currentGroup
    | r when r < 0 -> currentGroup
    | _ ->
        match currentCell with
        | c when c >= 128 -> currentGroup
        | c when c < 0 -> currentGroup
        | _ ->
            match disk.[currentRow].[currentCell] with
            | NotSet
            | Processed _ -> currentGroup
            | Unprocessed -> 
                disk.[currentRow].[currentCell] <- (Processed currentGroup)
                processCell currentRow (currentCell + 1) currentGroup |> ignore
                processCell (currentRow + 1) currentCell currentGroup |> ignore
                processCell currentRow (currentCell - 1) currentGroup |> ignore
                processCell (currentRow - 1) currentCell currentGroup |> ignore
                currentGroup + 1

let rec checkDisk (currentRow:int) (currentCell:int) (currentGroup:int) =
    match currentRow with
    | r when r >= 128 -> currentGroup - 1
    | _ ->
        match currentCell with
        | c when c >= 128 -> checkDisk (currentRow + 1) 0 currentGroup
        | _ ->  
            processCell currentRow currentCell currentGroup
            |> checkDisk currentRow (currentCell + 1)

checkDisk 0 0 1
