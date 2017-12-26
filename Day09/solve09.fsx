open System.IO

let input = 
    Path.Combine(__SOURCE_DIRECTORY__, "input09.txt")
    |> File.ReadAllText
    |> Seq.toList

type SearchState =
| ReadingGarbage
| Ignoring of SearchState
| Searching

type SearchResult = {
    groupsFound: string
    rest: char list
    searchStatus: SearchState
    countGarbage: int
}

let rec getGroups (sr:SearchResult) =
    match sr.rest with
    | [] -> (sr.groupsFound, sr.countGarbage)
    | c::r -> 
        match sr.searchStatus with
        | Ignoring ss -> {sr with searchStatus = ss; rest = r} |> getGroups
        | ReadingGarbage -> 
            match c with
            | '>' -> {sr with searchStatus = Searching; rest = r} |> getGroups
            | '!' -> {sr with searchStatus = (Ignoring ReadingGarbage); rest = r} |> getGroups
            | _ -> {sr with rest = r; countGarbage = sr.countGarbage + 1 } |> getGroups
        | Searching ->
            match c with
            | '<' -> {sr with searchStatus = ReadingGarbage; rest = r} |> getGroups
            | '!' -> {sr with searchStatus = (Ignoring Searching); rest = r} |> getGroups
            | '{' -> {sr with groupsFound = sr.groupsFound + "{"; rest = r} |> getGroups
            | '}' -> {sr with groupsFound = sr.groupsFound + "}"; rest = r} |> getGroups
            | _ -> {sr with rest = r} |> getGroups

let groups = { groupsFound = ""; rest = input; searchStatus = Searching; countGarbage = 0 } |> getGroups


type PointsState = {
    points: int list
    nextPoints: int
    rest: char list
}
let rec findPoints (ps:PointsState) =
    match ps.rest with
    | [] -> ps.points |> List.sum
    | c::r ->
        match c with
        | '{' -> {points = ps.nextPoints::ps.points; nextPoints = ps.nextPoints + 1; rest = r} |> findPoints
        | '}' -> {points = ps.points; nextPoints = ps.nextPoints - 1; rest = r} |> findPoints
        | _ -> failwith "WTF???"

{ points = []; nextPoints = 1; rest = (groups |> fst |> Seq.toList) } |> findPoints

