open System.IO
open System.Collections.Generic

type Operation = 
| Increment of int
| Decrement of int

type Criteria = {
    register: string
    operator: string
    number: int
}

type Input = {
    register: string
    operation: Operation
    criteria: Criteria
}

let parseLine (s:string) =
    let tokens = s.Split([|' '|])
    let criteria = {
        register = tokens.[4]
        operator = tokens.[5]
        number = tokens.[6] |> int
    }

    let operation = 
        match tokens.[1] with
        | "inc" -> tokens.[2] |> int |> Increment
        | "dec" -> tokens.[2] |> int |> Decrement
        | _ -> failwith "What?? Foolish input.."

    {
        register = tokens.[0]
        operation = operation
        criteria = criteria
    }

let input = 
    Path.Combine(__SOURCE_DIRECTORY__, "input08.txt")
    |> File.ReadAllLines
    |> Array.toList
    |> List.map parseLine

let regs = new Dictionary<string,int>()

input |> List.map (fun i -> i.register) |> List.distinct |> List.iter (fun r -> regs.Add(r, 0) )

let evaluateCriteria (c:Criteria) =
    match c.operator with 
    | "==" -> regs.[c.register] = c.number
    | ">=" -> regs.[c.register] >= c.number
    | "<=" -> regs.[c.register] <= c.number
    | "!=" -> regs.[c.register] <> c.number
    | ">" -> regs.[c.register] > c.number
    | "<" -> regs.[c.register] < c.number
    | s -> failwithf "What?? Foolish input...: %s" s

let processInput (i:Input) =
    if i.criteria |> evaluateCriteria then
        match i.operation with
        | Increment n -> regs.[i.register] <- (regs.[i.register] + n)
        | Decrement n -> regs.[i.register] <- (regs.[i.register] - n)
    regs.[i.register]

input |> List.map processInput |> List.max

regs |> Seq.maxBy (fun kvp -> kvp.Value)