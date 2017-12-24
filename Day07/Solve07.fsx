open System.IO

type SingleDisk = {
    name: string
    weight: int
}

type ComplexDisk = {
    disk: SingleDisk
    children: string list
}

type Line =
| Leaf of SingleDisk
| Branch of ComplexDisk

let trim (s:string) = s.Trim()

let parseDisk (s:string) =
    let name = s.Split([|' '|]).[0]
    let weight = s.Split([|'(';')'|]).[1] |> int
    { name = name; weight = weight }

let parseComplexDisk (s:string) =
    let diskString = s.Split([|'-'|]).[0]
    let disk = diskString |> parseDisk
    let l = s.Split([|'>'|]).[1].Split([|','|]) |> Array.toList |> List.map trim
    { disk = disk; children = l}

let parseLine (s:string) =
    if s.Contains("->") then
        s |> parseComplexDisk |> Branch
    else
        s |> parseDisk |> Leaf

let selectBranches (l:Line) =
    match l with
    | Leaf _ -> None
    | Branch b -> Some b

let input = 
    Path.Combine(__SOURCE_DIRECTORY__, "input07.txt")
    |> File.ReadAllLines
    |> Array.toList
    |> List.map parseLine

let allChildren (cs:Line list) =
    cs 
    |> List.choose selectBranches
    |> List.collect (fun c -> c.children) |> List.distinct

let children = input |> allChildren

let isChild (children:string list) (l:Line) =
    match l with
    | Leaf _ -> true
    | Branch b -> children |> List.contains b.disk.name
    
let isNotChild (children:string list) (l:Line) = not (isChild children l)

let root = input |> List.filter (isNotChild children) |> List.head

// Part 2

type WeightedBranch = {
    name: string
    ownWeight: int
    totalWeight: int
    isBalanced: bool
    children: WeightedBranch list
}

let getName (l:Line) =
    match l with
    | Leaf s -> s.name
    | Branch c -> c.disk.name

let namedInput = input |> List.map (fun l -> (getName l, l)) |> dict

let rec getWeighted (l:Line) =
    match l with
    | Leaf s -> { name = s.name
                  ownWeight = s.weight
                  totalWeight = s.weight
                  isBalanced = true
                  children = [] }
    | Branch c -> 
        let children = c.children |> List.map ((fun s -> namedInput.[s]) >> getWeighted)
        let childrensWeight = children |> List.map (fun b -> b.totalWeight)
        let isBalanced = childrensWeight |> List.distinct |> List.length |> (=) 1
        { name = c.disk.name
          ownWeight = c.disk.weight
          totalWeight = (childrensWeight |> List.sum) + c.disk.weight
          isBalanced = isBalanced
          children = children }

let rec getUnbalanced (w:WeightedBranch) =
    if w.isBalanced then
        { w with children = [] }
    else
        { w with children = (w.children |> List.map getUnbalanced) }

let rec printUnbalancedSummary (innrykk:int) (w:WeightedBranch) =
    let totaltInnrykk = (String.replicate innrykk "    ")
    printf "%s" totaltInnrykk
    printfn "%-7s: %6i, %6i" w.name w.totalWeight w.ownWeight
    w.children |> List.iter (printUnbalancedSummary (innrykk + 1)) 

root |> getWeighted |> getUnbalanced |> printUnbalancedSummary 0
