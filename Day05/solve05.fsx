open System.IO

let a =
    Path.Combine(__SOURCE_DIRECTORY__, "input05.txt")
    |> File.ReadAllLines
    |> Array.map int

type State = {
    a: int array
    steps: int
    current: int
}

let increment (a:int array) (c:int) =
    a.[c] <- a.[c] + 1

let getNewState state = 
    let nextCurrent = state.current + state.a.[state.current]
    let updatedSteps = state.steps + 1
    increment state.a state.current
    {state with steps=updatedSteps;current=nextCurrent}

let startState = {
    a = a
    steps = 0
    current = 0
}

let lengthOfA = startState.a |> Array.length  
let rec move state =
    let nextState = state |> getNewState
    match nextState.current >= lengthOfA with
    | true -> nextState.steps
    | false -> nextState |> move 

startState |> move