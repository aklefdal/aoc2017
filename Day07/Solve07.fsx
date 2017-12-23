open System.IO
open System

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
        s |> parseDisk |> Leaf
    else
        s |> parseComplexDisk |> Branch

let input = 
    Path.Combine(__SOURCE_DIRECTORY__, "input07.txt")
    |> File.ReadAllLines

input.[0] |> parseDisk
input.[1] |> parseComplexDisk
