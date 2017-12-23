open System.IO

let input () =
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

let decrement (a:int array) (c:int) =
    a.[c] <- a.[c] - 1

let getNewState state = 
    let nextCurrent = state.current + state.a.[state.current]
    let updatedSteps = state.steps + 1
    increment state.a state.current
    {state with steps=updatedSteps;current=nextCurrent}

let startState = {
    a = input()
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

// Step 2

let getNewStateStep2 state = 
    let nextCurrent = state.current + state.a.[state.current]
    let updatedSteps = state.steps + 1
    match state.a.[state.current] >= 3 with
    | true -> decrement state.a state.current
    | false -> increment state.a state.current
    {state with steps=updatedSteps;current=nextCurrent}

let startStateStep2 = {
    a = input()
    steps = 0
    current = 0
}

let lengthOfA2 = startStateStep2.a |> Array.length  

let rec moveStep2 state =
    let nextState = state |> getNewStateStep2
    match nextState.current >= lengthOfA2 with
    | true -> nextState.steps
    | false -> nextState |> moveStep2

startStateStep2 |> moveStep2
