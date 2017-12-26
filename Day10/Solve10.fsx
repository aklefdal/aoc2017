open System.IO

let splitOnComma (s:string) = s.Split([|','|]) 

let input = 
    Path.Combine(__SOURCE_DIRECTORY__, "input10.txt")
    |> File.ReadAllText
    |> splitOnComma
    |> Array.toList
    |> List.map int

let ring = seq {0..255} |> Seq.toArray
// let input = [3;4;1;5]

let rec getSection (current:int) (sectionLength:int) (section:int list) = 
    match sectionLength with
    | 0 -> section
    | _ -> 
        match current with
        | c when c >= ring.Length -> 
            getSection (1) (sectionLength - 1) (ring.[0]::section)
        | _ -> 
            getSection (current + 1) (sectionLength - 1) (ring.[current]::section)

let rec setSection (current:int) (section:int list) = 
    match section with
    | [] -> ()
    | h::t -> 
        match current with
        | c when c >= ring.Length ->
            ring.[0] <- h
            setSection 1 t
        | _ -> 
            ring.[current] <- h
            setSection (current + 1) t

let reverseSection (current:int) (skipLength:int) (sectionLength:int) =
    getSection current sectionLength [] |> setSection current
    let newPossibleCurrent = current + sectionLength + skipLength
    if newPossibleCurrent >= ring.Length then
        (newPossibleCurrent - ring.Length, skipLength + 1)
    else
        (newPossibleCurrent, skipLength + 1)    

let rec doWork (current:int) (skipLength:int) (rest:int list) =
    match rest with
    | [] -> ()
    | h::t ->
        let (newCurrent, newSkipLength) =  reverseSection current skipLength h
        doWork newCurrent newSkipLength t

doWork 0 0 input
ring.[0] * ring.[1]
