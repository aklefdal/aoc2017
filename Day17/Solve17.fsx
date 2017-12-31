let input = 376
// let input = 3

let rec findNextPos (l:int list) (currentPos:int) (count:int) =
    let pos = 
        match l.Length = currentPos with
        | true -> 0
        | false -> currentPos

    match count with
    | 0 -> pos + 1
    | _ -> findNextPos l (pos + 1) (count - 1)

let insertAtPos (l:int list) (pos:int) (value:int) =
    let beginningOfList = l |> List.take pos
    let endOfList = l |> List.skip pos
    [beginningOfList; [value]; endOfList] |> List.reduce (@)

let rec buildRing l pos value =
    let nextPos = findNextPos l pos input
    let newRing = insertAtPos l nextPos value
    match value with
    | 2017 -> newRing 
    | _ -> buildRing newRing nextPos (value + 1)

let ring = buildRing [0] 0 1
let pos2017 = ring |> List.findIndex (fun e -> e = 2017)
ring.[pos2017 + 1]

// Part 2
let rec findPos l max pos value =
    match max = value with
    | true -> l
    | false -> 
        let newPos = (pos + input) % value + 1
        findPos ((value,newPos)::l) max newPos (value + 1)

findPos [] 50000000 1 1 |> List.find (fun (_,pos) -> pos = 1) |> fst
